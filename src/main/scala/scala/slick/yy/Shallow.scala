package scala.slick.yy

object Shallow {

  class Query[T] {
    def map[S](projection: T => S): Query[S] = ???
    def filter(projection: T => Boolean): Query[T] = ???
    def withFilter(projection: T => Boolean): Query[T] = ???
    def sortBy[S](projection: T => S)(implicit ord: Ordering[S]): Query[T] = ???
    def sorted(implicit ord: Ordering[T]): Query[T] = ???
    def take(i: Int): Query[T] = ???
    def drop(i: Int): Query[T] = ???
    def toSeq: Seq[T] = ???
    def first: T = ???
  }

  object Query {
    def ofTable[T](i: Table[T]): Query[T] = ???
    def apply[T](i: T): Query[T] = ???
  }

  class Table[T] {

  }

  object Table {
    def test(): Table[TableRow] = ???
    def test2(): Table[TableARow] = ???
    def getTable[S]: Table[S] = ???
  }

  type TableRow = YYSlickCake#TableRow

  type TableARow = YYSlickCake#TableARow
}
