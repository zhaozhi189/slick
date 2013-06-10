package scala.slick.yy

import scala.slick.lifted.Column
import scala.slick.lifted.Projection
import scala.slick.lifted.Query
import scala.slick.lifted.ColumnOrdered
import scala.slick.jdbc.JdbcBackend
import scala.slick.ast.Node
import scala.slick.ast.Library
import scala.slick.lifted.FunctionSymbolExtensionMethods
import scala.slick.ast.StaticType
import FunctionSymbolExtensionMethods._
import scala.slick.ast.LiteralNode
import StaticType._
import scala.slick.ast.TypedType
import scala.slick.lifted.Shape
import scala.slick.lifted.IdentityShape
import scala.slick.lifted.ColumnExtensionMethods
import scala.slick.lifted.PlainColumnExtensionMethods
import scala.slick.lifted.OptionMapper2
import scala.slick.ast.BaseTypedType
import scala.slick.lifted.ConstColumn
import scala.slick.lifted.Rep
import scala.slick.lifted.AbstractTable
import scala.slick.lifted.NonWrappingQuery
import scala.slick.lifted.WrappingQuery
import scala.slick.lifted.CanBeQueryCondition
import scala.slick.driver.JdbcDriver
import scala.slick.driver.H2Driver
import scala.slick.lifted.{ Ordered => LOrdered }
import scala.slick.jdbc.UnitInvoker
import scala.slick.lifted.NumericColumnExtensionMethods
import scala.slick.lifted.StringColumnExtensionMethods
import scala.slick.SlickException
import scala.slick.lifted.BooleanColumnExtensionMethods
import scala.slick.profile.BasicDriver
import scala.slick.driver.JdbcProfile
import scala.slick.lifted.BaseJoinQuery
import scala.slick.lifted.SingleColumnQueryExtensionMethods
import scala.slick.lifted.OptionColumnExtensionMethods
import scala.slick.lifted.PlainColumnExtensionMethods

trait YYWrapper[UT] {
  type NonRep = UT
  def underlying: Rep[UT]
}

trait YYRep[T] extends YYWrapper[T]

object YYValue {
  def fromLifted[T](rep: Any): YYRep[T] =
    if (rep.isInstanceOf[Rep[_]])
      applyUntyped(rep.asInstanceOf[Rep[T]])
    else
      YYTuple.fromMixedTuple[Product](rep.asInstanceOf[Product]).asInstanceOf[YYRep[T]]

  def applyUntyped[T](rep: Rep[T]): YYRep[T] = {
    rep match {
      case c: Column[_] => YYColumn(c)
      case t: AbstractTable[_] => YYTable(t)
      case q: Query[_, _] => {
        type U = Nothing
        type Q = Query[Rep[U], U]
        YYQuery.fromQuery[U](q.asInstanceOf[Q]).asInstanceOf[YYRep[T]]
      }
      case tup: Projection[_] => YYProjection.fromLiftedProjection(tup.asInstanceOf[Projection[Product]]).asInstanceOf[YYRep[T]]
    }
  }

  def valueOfQuery[U, T <: Rep[U]](query: Query[T, U]): T = {
    import scala.language.reflectiveCalls
    type Unpackable = {
      val unpackable: scala.slick.lifted.ShapedValue[_, _]
    }
    val value = query.asInstanceOf[Unpackable].unpackable.value
    YYValue.fromLifted(value).underlying.asInstanceOf[T]
  }

  def apply[T](column: Column[T]): YYColumn[T] = {
    YYColumn(column)
  }

  def apply[T](table: AbstractTable[T]): YYTable[T] = {
    YYTable(table)
  }

  def apply[T, E <: YYRep[T]](rep: Rep[T]): E = {
    YYValue.applyUntyped(rep).asInstanceOf[E]
  }
}

class YYOption[T](val column: Column[Option[T]]) extends YYColumn[Option[T]] {
  override def underlying = column
  val optionColumnExtension = new OptionColumnExtensionMethods(column)

  //  def getOrElse(default: => YYColumn[T]): YYColumn[T] =
  def getOrElse[T1](default: => YYColumn[T]): YYColumn[T] =
    YYColumn(optionColumnExtension.getOrElse(default.getValue))
  def get: YYColumn[T] =
    YYColumn(optionColumnExtension.get)
}

object YYOption {
  def fromYYColumn[T](yyColumn: YYColumn[Option[T]]): YYOption[T] =
    new YYOption(yyColumn.underlying)

  def fromPlainColumn[T](column: Column[T]): YYOption[T] =
    new YYOption(new PlainColumnExtensionMethods(column).?)
}

trait YYColumn[T] extends ColumnExtensionOps[T] with ColumnNumericExtensionOps[T]
  with ColumnStringExtensionOps[T] with ColumnBooleanExtensionOps[T] with YYRep[T] {
  val column: Column[T]
  override def underlying = column
  def n = Node(column)
  implicit def om[T2, TR] = OptionMapper2.plain.asInstanceOf[OptionMapper2[T, T, TR, T, T2, TR]]
  def getValue: T = throw new SlickException("Accessing YYColumn value!")
}

// order stuff

class YYOrdering[T](val ord: Ordering[T], val isReverse: Boolean = false) { self =>
  def reverse: YYOrdering[T] =
    new YYOrdering(ord.reverse, !isReverse) {
      override def toOrdered(x: Rep[T]) = new LOrdered(self.toOrdered(x).columns.map {
        case (n, ord) => (n, ord.reverse)
      })
    }

  protected final def yyOrderingListToOrdered[U <: Product](x: Rep[U])(yyOrdProduct: Product): LOrdered =
    new LOrdered(YYOrdering.repToOrdered(x).columns.zip(yyOrdProduct.productIterator.toList.map(_.asInstanceOf[YYOrdering[_]])).map {
      case ((n, ord), yord) => (n, if (yord.isReverse) ord.reverse else ord)
    })

  def toOrdered(x: Rep[T]): LOrdered = YYOrdering.repToOrdered(x)
}

object YYOrdering extends YYOrderingTuples {
  def apply[T](ord: YYOrdering[T]): YYOrdering[T] =
    ord

  def by[T, S](f: YYRep[T] => YYRep[S])(ord: YYOrdering[S]): YYOrdering[T] = {
    val newOrd = Ordering.by({ (x: T) => f(YYConstColumn(x)(null /*FIXME*/ )).asInstanceOf[YYConstColumn[S]].value })(ord.ord)
    new YYOrdering[T](newOrd) {
      override def toOrdered(x: Rep[T]): LOrdered = {
        val newX = f(YYValue(x)).underlying
        YYOrdering.repToOrdered(newX)
      }
    }
  }

  def repToOrdered[T](rep: Rep[T]): LOrdered = {
    rep match {
      case column: Column[T] => column.asc
      case product: Product => new LOrdered(
        product.productIterator.flatMap { x =>
          repToOrdered(x.asInstanceOf[Rep[_]]).columns
        }.toSeq)
    }
  }

  def fromOrdering[T](ord: Ordering[T]): YYOrdering[T] =
    new YYOrdering(ord)

  val String = fromOrdering(Ordering.String)
  val Int = fromOrdering(Ordering.Int)
}

object YYColumn {
  def apply[T](c: Column[T]): YYColumn[T] = {
    c.tpe match {
      case o: scala.slick.ast.OptionType => new YYOption[Nothing](c.asInstanceOf[Column[Option[Nothing]]]).asInstanceOf[YYColumn[T]]
      case _ => new YYColumn[T] {
        val column = c
      }
    }
  }
}

trait YYTableRow

trait YYTable[T] extends YYRep[T] {
  val table: AbstractTable[T]
  override def underlying = table
}

object YYTable {
  def apply[T](t: AbstractTable[T]): YYTable[T] = {
    new YYTable[T] {
      val table = t
    }
  }
}

class YYConstColumn[T](val constColumn: ConstColumn[T]) extends YYColumn[T] {
  override val column = constColumn
  val value = constColumn.value
  override def getValue: T = value
}

object YYConstColumn {
  def apply[T: TypedType](v: T): YYColumn[T] = new YYConstColumn(ConstColumn[T](v))
}

trait ColumnExtensionOps[T] { self: YYColumn[T] =>

  def extendedColumn = new PlainColumnExtensionMethods(column)

  def isNull: YYColumn[Boolean] = YYColumn(extendedColumn.isNull)
  def isNotNull: YYColumn[Boolean] = YYColumn(extendedColumn.isNotNull)

  def is[T2](e: YYColumn[T2]): YYColumn[Boolean] = YYColumn(extendedColumn is e.column)
  def ===[T2](e: YYColumn[T2]): YYColumn[Boolean] = YYColumn(extendedColumn === e.column)
  def >[T2](e: YYColumn[T2]): YYColumn[Boolean] = YYColumn(extendedColumn > e.column)
  def <[T2](e: YYColumn[T2]): YYColumn[Boolean] = YYColumn(extendedColumn < e.column)
}
trait ColumnNumericExtensionOps[T] { self: YYColumn[T] =>

  lazy val numericColumn = new NumericColumnExtensionMethods[T, T](column)
  // FIXME add option mapper!
  def +(e: YYColumn[T]): YYColumn[T] = {
    if (e.column.tpe.equals(JdbcDriver.columnTypes.stringJdbcType))
      (this ++ e.asInstanceOf[YYColumn[String]]).asInstanceOf[YYColumn[T]]
    else
      YYColumn(numericColumn + e.column)
  }
  def -(e: YYColumn[T]): YYColumn[T] = YYColumn(numericColumn - e.column)
  def *(e: YYColumn[T]): YYColumn[T] = YYColumn(numericColumn * e.column)
  def /(e: YYColumn[T]): YYColumn[T] = YYColumn(numericColumn / e.column)
  def %(e: YYColumn[T]): YYColumn[T] = YYColumn(numericColumn % e.column)
  def abs = YYColumn(numericColumn.abs)
  def ceil = YYColumn(numericColumn.ceil)
  def floor = YYColumn(numericColumn.floor)
  def sign = YYColumn(numericColumn.sign)
  def toDegrees = YYColumn(numericColumn.toDegrees)
  def toRadians = YYColumn(numericColumn.toRadians)
}
trait ColumnStringExtensionOps[T] { self: YYColumn[T] =>

  lazy val stringColumn = new StringColumnExtensionMethods[String](column.asInstanceOf[Column[String]])
  // FIXME add option mapper!
  def length() = YYColumn(stringColumn.length)
  def like(e: YYColumn[String], esc: Char = '\0') = YYColumn(stringColumn.like(e.column, esc))
  def ++(e: YYColumn[String]) = YYColumn(stringColumn ++ e.column)
  def startsWith(s: YYColumn[String]) = YYColumn(stringColumn.startsWith(s.getValue))
  def endsWith(s: YYColumn[String]) = YYColumn(stringColumn.endsWith(s.getValue))
  def toUpperCase() = YYColumn(stringColumn.toUpperCase)
  def toLowerCase() = YYColumn(stringColumn.toLowerCase)
  def ltrim() = YYColumn(stringColumn.ltrim)
  def rtrim() = YYColumn(stringColumn.rtrim)
  def trim() = YYColumn(stringColumn.trim)
}

trait ColumnBooleanExtensionOps[T] { self: YYColumn[T] =>
  lazy val booleanColumn = new BooleanColumnExtensionMethods[Boolean](column.asInstanceOf[Column[Boolean]])

  def &&(b: YYColumn[Boolean]) = YYColumn(booleanColumn && b.column)
  def ||(b: YYColumn[Boolean]) = YYColumn(booleanColumn || b.column)
  def unary_! = YYColumn(!booleanColumn)
}

object YYShape {
  def ident[U] = Shape.impureShape.asInstanceOf[Shape[Rep[U], U, Rep[U]]]
}

trait YYQuery[U] extends QueryOps[U] with YYRep[Seq[U]] {
  val query: Query[Rep[U], U]
  def repValue: Rep[U] = query match {
    case nwq: NonWrappingQuery[_, _] => nwq.unpackable.value
    case wq: WrappingQuery[_, _] => wq.base.value
  }
  type E <: YYRep[U]
  def value: E = YYValue[U, E](repValue)
  override def underlying = query
  object BooleanRepCanBeQueryCondition extends CanBeQueryCondition[Rep[Boolean]] {
    def apply(value: Rep[Boolean]) = value.asInstanceOf[Column[Boolean]]
  }

  private def invoker(implicit driver: JdbcProfile): UnitInvoker[U] =
    driver.Implicit.queryToQueryInvoker(query)

  def first(implicit driver: JdbcProfile, session: JdbcBackend#Session): U =
    invoker.first
  def toSeq(implicit driver: JdbcProfile, session: JdbcBackend#Session): Seq[U] =
    invoker.list.toSeq

  def firstImplicit: (JdbcDriver => JdbcBackend#Session => U) = (driver: JdbcDriver) =>
    (session: JdbcBackend#Session) =>
      invoker(driver).first()(session)
  def toSeqImplicit: (JdbcDriver => JdbcBackend#Session => Seq[U]) = (driver: JdbcDriver) =>
    (session: JdbcBackend#Session) =>
      invoker(driver).list()(session).toSeq

  def getInvoker: (JdbcDriver => UnitInvoker[U]) = (driver: JdbcDriver) =>
    invoker(driver)
}

trait YYJoinQuery[U1, U2] extends YYQuery[(U1, U2)] {
  def on(pred: (YYRep[U1], YYRep[U2]) => YYColumn[Boolean]): YYQuery[(U1, U2)] = {
    def underlyingPredicate(_1: Rep[U1], _2: Rep[U2]): Column[Boolean] =
      pred(YYValue.applyUntyped(_1), YYValue.applyUntyped(_2)).underlying
    YYQuery.fromJoinQuery(query.asInstanceOf[BaseJoinQuery[Rep[U1], Rep[U2], U1, U2]].on[Column[Boolean]](underlyingPredicate _))
  }
}

object YYQuery {
  def create[U](q: Query[Rep[U], U], e: Rep[U]): YYQuery[U] = {
    class YYQueryInst[E1 <: YYRep[U]] extends YYQuery[U] {
      type E = E1
      val query = q
      override def repValue: Rep[U] = e
    }
    e match {
      case col: Column[U] => new YYQueryInst[YYColumn[U]] {}
      case tab: AbstractTable[U] => new YYQueryInst[YYTable[U]] {}
      case tupN: Projection[U] => new YYQueryInst[YYProjection[U]]
    }
  }

  def fromQuery[U](q: Query[Rep[U], U]): YYQuery[U] = {
    val e = YYValue.valueOfQuery(q)
    create(q, e)
  }
  def fromJoinQuery[U1, U2](q: Query[(Rep[U1], Rep[U2]), (U1, U2)]): YYJoinQuery[U1, U2] = {
    new YYJoinQuery[U1, U2] {
      type E = YYProjection[(U1, U2)]
      val query = q.asInstanceOf[Query[(Rep[(U1, U2)]), (U1, U2)]]
      override def repValue: Rep[(U1, U2)] = ???
    }
  }

  def apply[U](v: YYRep[U]): YYQuery[U] = {
    val query = Query(v.underlying)(YYShape.ident[U])
    create(query, v.underlying)
  }

  def apiApply[U <: YYRep[_]](v: U): YYQuery[v.NonRep] = {
    apply(v.asInstanceOf[YYRep[v.NonRep]])
  }

}

trait QueryOps[T] { self: YYQuery[T] =>
  protected def underlyingProjection[S](projection: YYRep[T] => YYRep[S]): Rep[T] => Rep[S] = {
    def underlyingProjection(x: Any): Rep[S] = projection({
      YYValue.fromLifted[T](x)
    }).underlying

    val res = underlyingProjection _
    res
  }
  def map[S](projection: YYRep[T] => YYRep[S]): YYQuery[S] = {
    val liftedResult = query.map(underlyingProjection(projection))(YYShape.ident[S])
    YYQuery.fromQuery(liftedResult)
  }
  def filter(projection: YYRep[T] => YYRep[Boolean]): YYQuery[T] = {
    val liftedResult = query.filter(underlyingProjection(projection))(BooleanRepCanBeQueryCondition)
    YYQuery.fromQuery(liftedResult)
  }
  def withFilter(projection: YYRep[T] => YYRep[Boolean]): YYQuery[T] =
    filter(projection)
  def flatMap[S](projection: YYRep[T] => YYQuery[S]): YYQuery[S] = {
    def qp(x: Rep[T]): Query[Rep[S], S] = projection({
      YYValue[T, E](x)
    }).query
    YYQuery.fromQuery(query.flatMap(qp))
  }

  def sortBy[S](f: YYRep[T] => YYRep[S])(ord: YYOrdering[S]): YYQuery[T] = {
    val newView = (x: Rep[S]) => ord.toOrdered(x)
    val liftedResult = query.sortBy(underlyingProjection(f))(newView)
    YYQuery.fromQuery(liftedResult)
  }
  def sorted(ord: YYOrdering[T]): YYQuery[T] = {
    val newView = (x: Rep[T]) => ord.toOrdered(x)
    val liftedResult = query.sorted(newView)
    YYQuery.fromQuery(liftedResult)
  }

  def take(i: YYColumn[Int]): YYQuery[T] = {
    val v = i.getValue
    YYQuery.fromQuery(query.take(v))
  }

  def drop(i: YYColumn[Int]): YYQuery[T] = {
    val v = i.getValue
    YYQuery.fromQuery(query.drop(v))
  }

  def length: YYColumn[Int] =
    YYColumn(query.length)

  def groupBy[S](f: YYRep[T] => YYRep[S]): YYQuery[(S, scala.slick.yy.Shallow.Query[T])] = {
    val liftedResult = query.groupBy(underlyingProjection(f))(YYShape.ident[S], YYShape.ident[T])
    YYQuery.fromQuery(liftedResult.asInstanceOf[Query[Rep[(S, scala.slick.yy.Shallow.Query[T])], (S, scala.slick.yy.Shallow.Query[T])]])
  }

  def innerJoin[S](q2: YYQuery[S]): YYJoinQuery[T, S] = {
    YYQuery.fromJoinQuery(query.innerJoin(q2.query))
  }

  // TODO needs option type
  //  def leftJoin[E2, U2](q2: Query[E2, U2]) = join(q2, JoinType.Left)
  def leftJoin[S](q2: YYQuery[S]): YYJoinQuery[T, S] = {
    YYQuery.fromJoinQuery(query.leftJoin(q2.query))
  }
  //  def rightJoin[E2, U2](q2: Query[E2, U2]) = join(q2, JoinType.Right)
  //  def outerJoin[E2, U2](q2: Query[E2, U2]) = join(q2, JoinType.Outer)
  def zip[S](q2: YYQuery[S]): YYJoinQuery[T, S] = {
    //    YYQuery.fromJoinQuery(query.zip(q2.query).asInstanceOf[Query[Rep[(T, S)], (T, S)]])
    YYQuery.fromJoinQuery(query.zip(q2.query))
  }
  def zipWithIndex: YYJoinQuery[T, Long] = YYQuery.fromJoinQuery(query.zipWithIndex)
}

final class YYSingleColumnQuery[T](val q: YYQuery[T]) extends AnyVal {
  def singleColumnQuery = new SingleColumnQueryExtensionMethods[T, T](q.underlying.asInstanceOf[Query[Column[T], T]])
  type OptionTM = TypedType[Option[T]]
  def min(implicit tm: OptionTM): YYOption[T] = {
    new YYOption(singleColumnQuery.min)
  }
  def max(implicit tm: OptionTM): YYOption[T] = {
    new YYOption(singleColumnQuery.max)
  }
  def avg(implicit tm: OptionTM): YYOption[T] = {
    new YYOption(singleColumnQuery.avg)
  }
  def sum(implicit tm: OptionTM): YYOption[T] = {
    new YYOption(singleColumnQuery.sum)
  }
}

object YYDebug {
  def apply(a: Any) {
    System.err.println(a)
  }
}
