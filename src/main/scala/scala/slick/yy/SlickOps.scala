package scala.slick.yy

import scala.slick.lifted.Column
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

trait SlickOps {

}

trait YYWraper[UT] {
  def underlying: Rep[UT]
}

trait YYRep[T] extends YYWraper[T]

object YYValue {
  def applyUntyped[T](rep: Rep[T]): YYRep[T] = {
    rep match {
      case c: Column[_] => YYColumn(c)
      case t: AbstractTable[_] => YYTable(t)
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
  implicit def implicitTpe: TypedType[T]
  def om[T2, TR] = OptionMapper2.plain.asInstanceOf[OptionMapper2[T, T, TR, T, T2, TR]]
}

object YYColumn {
  def apply[T](c: Column[T]): YYColumn[T] = new YYColumn[T] {
    val column = c
    override implicit def implicitTpe = c.tpe
  }
}

trait YYTable[T] extends YYRep[T] {
  val table: AbstractTable[T]
  override def underlying = table
}

object YYTable {
  def apply[T](t: AbstractTable[T]): YYTable[T] = new YYTable[T] {
    val table = t
  }
}

object YYConstColumn {
  def apply[T: TypedType](v: T): YYColumn[T] = YYColumn(new ConstColumn[T](v))
}

trait ColumnOps[T] { self: YYColumn[T] =>

  def isNull: YYColumn[Boolean] = YYColumn(extendedColumn.isNull)
  def isNotNull: YYColumn[Boolean] = YYColumn(extendedColumn.isNotNull)

  def is[T2](e: YYColumn[T2]): YYColumn[Boolean] =
    //    Library.==.column(n, Node(e.column))(implicitTpe)
    YYColumn(extendedColumn.is[T2, Boolean](e.column)(om[T2, Boolean]))
  def ===[T2](e: YYColumn[T2]): YYColumn[Boolean] =
    //    Library.==.column(n, Node(e.column))(implicitTpe)
    YYColumn(extendedColumn.===[T2, Boolean](e.column)(om[T2, Boolean]))
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
  //  def repToYY(rep: Rep[U]): YYRep[U] =
  //    rep match {
  //      case col: Column[U] => YYColumn(col)
  //      case tab: AbstractTable[U] => YYTable(tab)
  //    }
  implicit def implicitShape = YYShape.ident[U]
  override def underlying = query
}

object YYQuery {
  def apply[U](q: Query[Rep[U], U]): YYQuery[U] = {
    //    YYDebug("YYQuery apply started")
    val e = YYUtils.valueOfQuery(q)
    //    YYDebug(e)
    class YYQueryInst[E1 <: YYRep[U]] extends YYQuery[U] {
      type E = E1
      val query = q
      override def repValue: Rep[U] = e
    }
    e match {
      case col: Column[U] => new YYQueryInst[YYColumn[U]] {}
      case tab: AbstractTable[U] => new YYQueryInst[YYTable[U]] {}
    }
  }

  def apply[U](v: YYRep[U]): YYQuery[U] = {
    val query = Query(v.underlying)(YYShape.ident[U])
    YYQuery(query)
  }
}

trait QueryOps[T] { self: YYQuery[T] =>
  //  class Typer[S, S1]
  //  implicit def typer[S] = new Typer[S, YYRep[S]]
  //  implicit def rep[S] = new YYRep[S] {}
  //  def map[S](projection: YYRep[T] => YYRep[S]): YYQuery[S] = {
  //  def map[S](projection: E => YYRep[S]): YYQuery[S] = {
  def map[S](projection: E => YYRep[S]): YYQuery[S] = {
    //  def map[S: YYRep](projection: E => YYRep[S]): YYQuery[S] = {
    //    YYDebug("Map started")
    def underlyingProjection(x: Rep[T]): Rep[S] = projection({
      //      repToYY(x)
      YYValue[T, E](x)
    }).underlying
    val liftedResult = query.map(underlyingProjection)(YYShape.ident[S])
    //    YYDebug(liftedResult)
    YYQuery(liftedResult)
  }
  //  def flatMap[S](projection: YYRep[T] => YYQuery[S]): YYQuery[S] = {
  //    def qp(x: YYRep[T]): Query[Rep[S], S] = projection(x).query
  //    YYQuery(query.flatMap(qp))
  //  }
}

object YYUtils {
  def valueOfQuery[U, T <: Rep[U]](query: Query[T, U]): T = query match {
    case nwq: NonWrappingQuery[_, _] => nwq.unpackable.value
    case wq: WrappingQuery[_, _] => wq.base.value
  }
}

object YYDebug {
  def apply(a: Any) {
    System.err.println(a)
  }
}