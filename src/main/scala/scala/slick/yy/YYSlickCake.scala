package scala.slick.yy

import scala.{ Int => SInt }

trait YYSlickCake {
  type Tuple2[T1, T2] = YYProjection2[T1, T2]
  type Column[T] = YYColumn[T]
  type Table[T] = YYTable[T]
  //  type Seq[T] = YYQuery[T]
  type Query[T] = YYQuery[T]
  type Int = YYColumn[SInt]
  //  type Int = YYRep[SInt]
  type Long = YYColumn[scala.Long]
  type Double = YYColumn[scala.Double]
  type String = YYColumn[Predef.String]
  type Boolean = YYColumn[scala.Boolean]
  type TableRow = scala.slick.yy.YYTableRow

  type YYTableRow = Table[TableRow]

  implicit def fixClosureContraVariance[T, U <: YYRep[T], S](x: U => S) =
    //  implicit def fixClosureContraVarianceColumn[T, S](x: YYColumn[T] => S) =
    x.asInstanceOf[YYRep[T] => S]

  //  implicit def fixHack(x: YYTableRow) =
  //    x.asInstanceOf[YYRep[TableRow]]

  //  implicit def fixClosureContraVarianceTableRow[S](x: YYTableRow => S) =
  //    x.asInstanceOf[YYRep[TableRow] => S]

  //  implicit def fixClosureContraVarianceTable[T, S](x: YYTable[T] => S) =
  //    x.asInstanceOf[YYRep[T] => S]

  //  val Query = YYQuery
  object Query {
    //    def apply[T <: YYRep[_]](v: T) = YYQuery.apiApply(v)
    def apply[T](v: YYRep[T]): YYQuery[T] = YYQuery.apply(v)
    def ofTable[T](t: YYTable[T]): YYQuery[T] = YYQuery.apply(t)
  }
  //  type CanBuildFrom[A, B, C] = Null
  //  object Seq {
  //    def canBuildFrom[T]: CanBuildFrom[Any, Any, Any] = null
  //    def apply[T <: YYRep[_]](v: T*) = YYQuery.apiApply(v.head)
  //  }

  // TODO this is only for testing. Should be fixed
  def __ifThenElse[T](c: => Boolean, t: T, e: T) = t

  def __equals[T](t: Column[T], e: Column[T]) = t === e

  object Table {
    //    def test() = TestTable.YYTableA
    def test(): Table[TableRow] = TestTable.YYTableA.asInstanceOf[Table[TableRow]]
  }

  object Tuple2 {
    def apply[T1, T2](_1: Column[T1], _2: Column[T2]) = YYProjection.fromYY(_1, _2)
  }

  object TestTable {
    import scala.slick.driver.H2Driver.simple
    import scala.slick.driver.H2Driver.Implicit._
    //    class TableA extends simple.Table[(SInt, SInt)]("TABLE_A") {
    class TableA extends simple.Table[YYTableARow]("TABLE_A") {
      def id = column[SInt]("A_ID")
      def grade = column[SInt]("A_GRADE")
      def * = id ~ grade <> (YYTableARow, YYTableARow.unapply _)
    }
    object TableA extends TableA

    class YYTableARow(override val _1: SInt, override val _2: SInt) extends (SInt, SInt)(_1, _2) with TableRow
    object YYTableARow extends ((SInt, SInt) => YYTableARow) {
      def apply(v1: SInt, v2: SInt): YYTableARow = new YYTableARow(v1, v2)
      def unapply(param: YYTableARow): Option[YYTableARow] = Some(param)
    }

    //    class YYTableA extends Table[(SInt, SInt)] with YYProjection2[SInt, SInt] {
    class YYTableA extends Table[YYTableARow] {
      val table = TableA

      def id = YYColumn(table.id)
      def grade = YYColumn(table.grade)
      //      def _1 = id
      //      def _2 = grade
      override def toString = "YYTableA"
    }

    object YYTableA extends YYTableA

    YYTable.add(TableA, YYTableA)

    def underlying[E](x: YYRep[E]): TableA.type = x.underlying.asInstanceOf[TableA.type]
  }
}

