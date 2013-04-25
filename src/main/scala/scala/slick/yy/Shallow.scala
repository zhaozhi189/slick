package scala.slick.yy

object Shallow {

  class Query[T] {
    def map[S](projection: T => S): Query[S] = ???
    def filter(projection: T => Boolean): Query[T] = ???
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
    def test(): Table[(Int, Int)] = ???
  }
}