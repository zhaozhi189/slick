package scala.slick.yy

import scala.{ Int => SInt }
import scala.language.implicitConversions

trait YYSlickCake {
  type Tuple2[T1, T2] = YYProjection2[T1, T2]
  type Column[T] = YYColumn[T]
  type Table[T] = YYTable[T]
  type Query[T] = YYQuery[T]
  type Int = YYColumn[SInt]
  type Long = YYColumn[scala.Long]
  type Double = YYColumn[scala.Double]
  type String = YYColumn[Predef.String]
  type Boolean = YYColumn[scala.Boolean]
  type TableRow = scala.slick.yy.YYTableRow
  type YYTableRow = Table[TableRow] // w/o it: "type YYTableRow is not a member of CAKE"

  implicit def fixClosureContraVariance[T, U <: YYRep[T], S](x: U => S) =
    x.asInstanceOf[YYRep[T] => S]

  object Query {
    def apply[T](v: YYRep[T]): YYQuery[T] = YYQuery.apply(v)
    def ofTable[T](t: YYTable[T]): YYQuery[T] = YYQuery.apply(t)
  }

  def __ifThenElse[T](c: => Boolean, t: T, e: T) = t // TODO this is only for testing. Should be fixed

  def __equals[T](t: Column[T], e: Column[T]) = t === e

  object Tuple2 {
    def apply[T1, T2](_1: Column[T1], _2: Column[T2]) = YYProjection.fromYY(_1, _2)
  }

  // testing stuffs

  type TableARow = scala.slick.yy.YYTableARow
  type YYTableARow = Table[TableARow] // w/o it: "type YYTableARow is not a member of CAKE"

  case class TableARowWrapper(underlying: TestTable.TableA) {
    def id = YYColumn(underlying.id)
    def grade = YYColumn(underlying.grade)
  }

  implicit def convertYYTableARow(t: YYTableARow): TableARowWrapper =
    TableARowWrapper(t.underlying.asInstanceOf[TestTable.TableA])

  object Table {
    def test(): Table[TableRow] = TestTable.YYTableA.asInstanceOf[Table[TableRow]]
    def test2(): Table[TableARow] = TestTable.YYTableA
  }

}

object TestTable {
  import scala.slick.driver.H2Driver.simple
  import scala.slick.driver.H2Driver.Implicit._

  class TableA extends simple.Table[YYTableARow]("TABLE_A") {
    def id = column[SInt]("A_ID")
    def grade = column[SInt]("A_GRADE")
    def * = id ~ grade <> (YYTableARow, YYTableARow.unapply _)
  }
  object TableA extends TableA

  implicit def convertTuple2ToTableARow(tup2: (scala.Int, scala.Int)): YYTableARow =
    YYTableARow(tup2._1, tup2._2)

  class YYTableA extends YYTable[YYTableARow] {
    val table = TableA

    def id = YYColumn(table.id)
    def grade = YYColumn(table.grade)
    override def toString = "YYTableA"
  }

  object YYTableA extends YYTableA

  def underlying[E](x: YYRep[E]): TableA.type = x.underlying.asInstanceOf[TableA.type]
}

case class YYTableARow(val id: SInt, val grade: SInt) extends YYTableRow