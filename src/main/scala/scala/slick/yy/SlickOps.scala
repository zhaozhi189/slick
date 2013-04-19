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
  def value: Rep[U] = query match {
    case nwq: NonWrappingQuery[_, _] => nwq.unpackable.value
    case wq: WrappingQuery[_, _] => wq.base.value
  }
  def repToYY(rep: Rep[U]): YYRep[U] =
    rep match {
      case col: Column[U] => YYColumn(col)
      case tab: AbstractTable[U] => YYTable(tab)
    }
  implicit def implicitShape = YYShape.ident[U]
  override def underlying = query
}

object YYQuery {
  def apply[U](q: Query[Rep[U], U]) =
    new YYQuery[U] with QueryOps[U] {
      val query = q.asInstanceOf[Query[Rep[U], U]]
    }
  def apply[E](e: Rep[E]) =
    new YYQuery[E] with QueryOps[E] {
      val query = Query(e)(YYShape.ident[E])
      override def value: Rep[E] = e
    }
}

trait QueryOps[T] { self: YYQuery[T] =>
  def map[S](projection: YYRep[T] => YYRep[S]): YYQuery[S] = {
    def underlyingProjection(x: Rep[T]): Rep[S] = projection({
      repToYY(x)
    }).underlying
    YYQuery(query.map(underlyingProjection)(YYShape.ident[S]))
  }
  //  def flatMap[S](projection: YYRep[T] => YYQuery[S]): YYQuery[S] = {
  //    def qp(x: YYRep[T]): Query[Rep[S], S] = projection(x).query
  //    YYQuery(query.flatMap(qp))
  //  }
}