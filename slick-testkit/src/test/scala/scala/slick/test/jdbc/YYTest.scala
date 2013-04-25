package scala.slick.test.jdbc

import org.junit.Test
import org.junit.Assert._
import scala.slick.yy._

class YYTest {
  class Query[T] {
    def map[S](projection: T => S): Query[S] = ???
  }

  object Query {
    def apply[T](i: T): Query[T] = ???
  }

  @Test def simpleTest() {
    slickYYDebug {
      //    slickYY {
      //      val x = true
      //      val y =
      //        if (x)
      //          4.2
      //        else
      //          5.3
      val y = 5
      val q = Query(y)
      q.map(x => x)
      1 < y
    }
  }
}