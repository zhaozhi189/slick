package scala.slick.yy

import scala.slick.ast.Dump

object YYMain extends NumericOps with TupleOps {

  def main(args: Array[String]): Unit = {
    val a = NumInt(2)
    val b = NumInt(5)
    val c = a + b
    Dump(c)
    val tuple1 = new YYTuple2(a, b)
    Dump(tuple1)
    Dump(tuple1._1)
    val tuple2 = new YYTuple2(c, a)
  }

}