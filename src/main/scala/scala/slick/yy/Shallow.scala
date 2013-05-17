package scala.slick.yy

import scala.slick.driver.JdbcDriver
import scala.slick.jdbc.UnitInvoker
import scala.slick.jdbc.JdbcBackend
import scala.slick.driver.H2Driver

object Shallow {

  type Invoker[T] = (JdbcDriver => UnitInvoker[T])

  object Queryable {
    def apply[T]: Query[T] = ???
  }

  class Query[T] {
    def flatMap[S](projection: T => Query[S]): Query[S] = ???
    def map[S](projection: T => S): Query[S] = ???
    def filter(projection: T => Boolean): Query[T] = ???
    def withFilter(projection: T => Boolean): Query[T] = ???
    def length: Query[Int] = ???
    def sortBy[S](projection: T => S)(implicit ord: Ordering[S]): Query[T] = ???
    def sorted(implicit ord: Ordering[T]): Query[T] = ???
    def take(i: Int): Query[T] = ???
    def drop(i: Int): Query[T] = ???
    def toSeq: Seq[T] = ???
    //    def toSeq(implicit driver: JdbcDriver): Seq[T] = ???
    //    def toSeqSession(implicit driver: JdbcDriver): Seq[T] = ???
    def first: T = ???
    def getInvoker: Invoker[T] = ???
    def firstImplicit: (JdbcDriver => JdbcBackend#Session => T) = ???
    def toSeqImplicit: (JdbcDriver => JdbcBackend#Session => Seq[T]) = ???
  }

  implicit def stringWrapper(value: String): ColumnOps[String] =
    new ColumnOps(value)

  implicit class ColumnOps[T](value: T) {
    //    def abs: T = ??? // there's no need for it, intWrapper is handling it
    def ceil: T = ???
    def floor: T = ???
    def sign: T = ???
    def toDegrees: T = ???
    def toRadians: T = ???
    def ++(o: String): String = ???
    def like(o: String): Boolean = ???
    def ltrim: String = ???
    def rtrim: String = ???
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

  object TestH2 {
    implicit val h2Driver = H2Driver
    implicit def h2Session = YYUtils.h2Session
  }
}
