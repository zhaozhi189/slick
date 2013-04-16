package scala.slick.yy

import scala.slick.ast.Dump

object YYMain extends NumericOps {

  def main(args: Array[String]): Unit = {
    val a = NumInt(2)
    val b = NumInt(5)
    val c = a + b
    Dump(c)
  }

}