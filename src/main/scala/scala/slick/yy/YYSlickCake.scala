package scala.slick.yy

import scala.{ Int => SInt }

trait YYSlickCake {
  type Tuple2[T1, T2] = YYProjection2[T1, T2]
  type Column[T] = YYColumn[T]
  type Table[T] = YYTable[T]
  type Seq[T] = YYQuery[T]
  type Int = YYColumn[SInt]
  type Long = YYColumn[scala.Long]
  type Double = YYColumn[scala.Double]
  type String = YYColumn[Predef.String]
  type Boolean = YYColumn[scala.Boolean]

  object TestTable {
    import scala.slick.driver.H2Driver.simple
    import scala.slick.driver.H2Driver.Implicit._
    class TableA extends simple.Table[(SInt, SInt)]("TABLE_A") {
      def id = column[SInt]("A_ID")
      def grade = column[SInt]("A_GRADE")
      def * = id ~ grade
    }
    object TableA extends TableA

    class YYTableA extends Table[(SInt, SInt)] {
      val table = TableA

      def id = YYColumn(table.id)
      def grade = YYColumn(table.grade)
    }

    object YYTableA extends YYTableA

    def underlying[E](x: YYRep[E]): TableA.type = x.underlying.asInstanceOf[TableA.type]
  }
}

