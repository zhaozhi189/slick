package scala.slick.test.jdbc

import org.junit.Test
import org.junit.Assert._
import scala.slick.yy._

class YYTest {

  @Test def simpleTest() {
    val z = 3
    slickYYDebug {
      //    slickYY {
      val x = true
      val y =
        //        if (x)
        //          4.2
        //        else
        //          5.3
        5.3
      val q = Query(y)
      val q2 = q.map(x => x)
      val q3 = q2.filter(x => x < 1)
      val q4 = q2.filter(x => x > 1)
      val qz = Query(z)
      val q5 = qz.filter(x => x > 2.5)
      println(q3.toSeq)
      println(q4.toSeq)
      println(q5.toSeq)
      //      val q2 = Query(ta)
      1 < y
    }
  }

  @Test def tuple2Test() {
    slickYYDebug {
      val x = (1, 2)
      val q = Query(x)
      val q2 = q.map(x => x._2)
      val q3 = q2.filter(x => x == 1)
      val q4 = q2.filter(x => x > 1)
      val q5 = q2.filter(x => x == 2)
      println(x)
      println(q.toSeq)
      println(q2.toSeq)
      println(q3.toSeq)
      println(q4.toSeq)
      println(q5.toSeq)
    }
  }

  /*@Test def testTableTest() {
    //    val ta = YYTableA
    slickYYDebug {
      val tbl = Table.test()
      val q = Query.ofTable(tbl)
      //      val q2 = q.map((x: Table[(Int, Int)]) => x)
      val q2 = q.map(x => x)
      println(q2.toSeq)
    }
  }
  */
}

class Query[T] {
  def map[S](projection: T => S): Query[S] = ???
  def filter(projection: T => Boolean): Query[T] = ???
  def toSeq: Seq[T] = ???
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