package scala.slick.yy

object Shallow {

  class Query[T] {
    def map[S](projection: T => S): Query[S] = ???
    def filter(projection: T => Boolean): Query[T] = ???
    def toSeq: Seq[T] = ???
    def first: T = ???
    //    def tableMap[U <: Table[T], S](projection: U => S): Query[S] = ???
  }

  class QueryTable[T] {
    def map[S](projection: T => S): Query[S] = ???
    def filter(projection: T => Boolean): QueryTable[T] = ???
    def toSeq: Seq[T] = ???
    def first: T = ???
  }

  object Query {
    def ofTable2[T, U](t: U): QueryTable[T] = ???
    def ofTable[T](i: Table[T]): Query[T] = ???
    def apply[T](i: T): Query[T] = ???
  }

  class Table[T] {

  }

  object Table {
    def test(): Table[TableRow] = ???
    def test2(): Table[TableARow] = ???
    def getTable[S]: Table[S] = ???
    //    def get[S <: Table[_]]: S = ???
  }

  type TableRow = YYSlickCake#TableRow

  type TableARow = YYSlickCake#TableARow
}