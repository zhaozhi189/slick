package scala.slick.test.jdbc

import org.junit.Test
import org.junit.Assert._
import scala.slick.yy._

class YYTest {
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
      1 < y
    }
  }
}