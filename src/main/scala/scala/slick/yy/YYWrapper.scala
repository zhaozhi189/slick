package scala.slick.yy

import scala.slick.lifted.Column
import scala.slick.lifted.Projection
import scala.slick.lifted.Projection2
import scala.slick.lifted.Query
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

trait YYWraper[UT] {
  type NonRep = UT
  def underlying: Rep[UT]
}

trait YYRep[T] extends YYWraper[T]

object YYValue {
  def applyUntyped[T](rep: Rep[T]): YYRep[T] = {
    rep match {
      case c: Column[_] => YYColumn(c)
      case t: AbstractTable[_] => YYTable(t)
      case tup: Projection2[_, _] => YYProjection(tup)
    }
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

trait YYColumn[T] extends ColumnOps[T] with YYRep[T] {
  val column: Column[T]
  override def underlying = column
  def extendedColumn = new PlainColumnExtensionMethods(column)
  def n = Node(column)
  implicit def om[T2, TR] = OptionMapper2.plain.asInstanceOf[OptionMapper2[T, T, TR, T, T2, TR]]
}

object YYColumn {
  def apply[T](c: Column[T]): YYColumn[T] = new YYColumn[T] {
    val column = c
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

object YYConstColumn {
  def apply[T: TypedType](v: T): YYColumn[T] = YYColumn(ConstColumn[T](v))
}

trait ColumnOps[T] { self: YYColumn[T] =>

  def isNull: YYColumn[Boolean] = YYColumn(extendedColumn.isNull)
  def isNotNull: YYColumn[Boolean] = YYColumn(extendedColumn.isNotNull)

  def is[T2](e: YYColumn[T2]): YYColumn[Boolean] =
    YYColumn(extendedColumn is e.column)
  def ===[T2](e: YYColumn[T2]): YYColumn[Boolean] =
    YYColumn(extendedColumn === e.column)
  def >[T2](e: YYColumn[T2]): YYColumn[Boolean] =
    YYColumn(extendedColumn > e.column)
  def <[T2](e: YYColumn[T2]): YYColumn[Boolean] =
    YYColumn(extendedColumn < e.column)
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

  // FIXME
  implicit def session = YYUtils.session

  def first: U = JdbcDriver.Implicit.queryToQueryInvoker(query).first
  def toSeq: Seq[U] = JdbcDriver.Implicit.queryToQueryInvoker(query).list.toSeq
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
    val e = YYUtils.valueOfQuery(q)
    create(q, e)
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
  private def underlyingProjection[S](projection: E => YYRep[S]): Rep[T] => Rep[S] = {
    def underlyingProjection(x: Rep[T]): Rep[S] = projection({
      YYValue[T, E](x)
    }).underlying

    val res = underlyingProjection _
    res
  }

  def map[S](projection: E => YYRep[S]): YYQuery[S] = {
    val liftedResult = query.map(underlyingProjection(projection))(YYShape.ident[S])
    YYQuery.fromQuery(liftedResult)
  }
  def filter(projection: E => YYRep[Boolean]): YYQuery[T] = {
    val liftedResult = query.filter(underlyingProjection(projection))(BooleanRepCanBeQueryCondition)
    YYQuery.fromQuery(liftedResult)
  }
  def flatMap[S](projection: E => YYQuery[S]): YYQuery[S] = {
    def qp(x: Rep[T]): Query[Rep[S], S] = projection({
      YYValue[T, E](x)
    }).query
    YYQuery.fromQuery(query.flatMap(qp))
  }

  def length: YYColumn[Int] =
    YYColumn(query.length)
}

sealed trait YYProjection[T <: Product] extends YYRep[T] with Product {
  def canEqual(that: Any): Boolean = that.isInstanceOf[YYProjection[_]]
}

object YYProjection {
  def apply[T1, T2](tuple2: Projection2[T1, T2]): YYProjection2[T1, T2] = {
    new YYProjection2[T1, T2] {
      def _1 = YYColumn(tuple2._1)
      def _2 = YYColumn(tuple2._2)
      override def underlying = _1.underlying ~ _2.underlying
    }
  }

  def apply[T1, T2](_1: Column[T1], _2: Column[T2]): YYProjection2[T1, T2] = {
    apply(_1 ~ _2)
  }

  def fromYY[T1, T2](_1: YYColumn[T1], _2: YYColumn[T2]): YYProjection2[T1, T2] = {
    apply(_1.underlying ~ _2.underlying)
  }
}

trait YYProjection2[T1, T2] extends Product2[YYColumn[T1], YYColumn[T2]] with YYProjection[(T1, T2)] {
  override def toString = "YY(" + _1 + ", " + _2 + ")"
}

object YYUtils {

  def valueOfQuery[U, T <: Rep[U]](query: Query[T, U]): T = query match {
    case nwq: NonWrappingQuery[_, _] => nwq.unpackable.value
    case wq: WrappingQuery[_, _] => wq.base.value
  }

  // FIXME hack!
  val conn = H2Driver.simple.Database.forURL("jdbc:h2:mem:test1", driver = "org.h2.Driver")
  val session = conn.createSession
  def provideSession: JdbcDriver.Backend#Session = session
}

object YYDebug {
  def apply(a: Any) {
    System.err.println(a)
  }
}