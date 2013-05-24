package scala.slick.yy

import scala.language.implicitConversions
import scala.slick.driver.JdbcDriver
import scala.slick.jdbc.UnitInvoker
import scala.slick.jdbc.JdbcBackend
import scala.slick.driver.H2Driver

object Shallow {

  type Invoker[T] = (JdbcDriver => UnitInvoker[T])

  object Queryable {
    def apply[T]: Query[T] = ???
  }

  // TODO needs option type
  //  implicit class SingleColumnQuery[T <: AnyVal](val query: Query[T]) extends AnyVal {
  //    def min: T = ???
  //    def max: T = ???
  //    def avg: T = ???
  //    def sum: T = ???
  //  }

  class Query[T] {
    def flatMap[S](projection: T => Query[S]): Query[S] = ???
    def map[S](projection: T => S): Query[S] = ???
    def filter(projection: T => Boolean): Query[T] = ???
    def withFilter(projection: T => Boolean): Query[T] = ???
    def length: Int = ???
    def sortBy[S](projection: T => S)(implicit ord: Ordering[S]): Query[T] = ???
    def sorted(implicit ord: Ordering[T]): Query[T] = ???
    def groupBy[S](f: T => S): Query[(S, Seq[T])] = ???
    def innerJoin[S](q2: Query[S]): JoinQuery[T, S] = ???
    def zip[S](q2: Query[S]): JoinQuery[T, S] = ???
    def zipWithIndex: JoinQuery[T, Long] = ???
    def take(i: Int): Query[T] = ???
    def drop(i: Int): Query[T] = ???
    def toSeq: Seq[T] = ???
    def first: T = ???
    def getInvoker: Invoker[T] = ???
    def firstImplicit: (JdbcDriver => JdbcBackend#Session => T) = ???
    def toSeqImplicit: (JdbcDriver => JdbcBackend#Session => Seq[T]) = ???
  }

  class JoinQuery[T1, T2] extends Query[(T1, T2)] {
    def on(pred: (T1, T2) => Boolean): Query[(T1, T2)] = ???
  }

  implicit def stringWrapper(value: String): ColumnOps[String] =
    new ColumnOps(value)

  implicit class ColumnOps[T](val value: T) extends AnyVal {
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
    def getTable[S]: Table[S] = ???
  }

  object TestH2 {
    implicit val h2Driver = H2Driver
    implicit def h2Session = _session
    private val conn = h2Driver.simple.Database.forURL("jdbc:h2:mem:test14", driver = "org.h2.Driver")
    private var _session = conn.createSession
    def provideSession: JdbcBackend#Session = _session
    def closeSession {
      _session.close
      _session = conn.createSession
    }
  }
}
