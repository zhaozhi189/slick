package scala.slick.yy

trait YYSlickCake {
  type Tuple2[T1, T2] = YYProjection2[T1, T2]
  type Column[T] = YYColumn[T]
  type Table[T] = YYTable[T]
  type Query[T] = YYQuery[T]

  object TestTable {
    import scala.slick.driver.H2Driver.simple
    import scala.slick.driver.H2Driver.Implicit._
    class TableA extends simple.Table[(Int, Int)]("TABLE_A") {
      def id = column[Int]("A_ID")
      def grade = column[Int]("A_GRADE")
      def * = id ~ grade
    }
    object TableA extends TableA

    class YYTableA extends Table[(Int, Int)] {
      val table = TableA

      def id = YYColumn(table.id)
      def grade = YYColumn(table.grade)
    }

    object YYTableA extends YYTableA

    def underlying[E](x: YYRep[E]): TableA.type = x.underlying.asInstanceOf[TableA.type]
  }
}