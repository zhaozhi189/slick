package scala.slick.test.jdbc

import org.junit.Test
import org.junit.Assert._
import scala.slick.yy._

class YYTest {
  @Test def simpleTest() {
    slickYY {
      1 + 2
    }
  }
}