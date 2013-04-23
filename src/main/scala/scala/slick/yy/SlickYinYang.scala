package scala.slick.yy

import ch.epfl.lamp.yinyang.api.BaseYinYang

trait SlickYinYang extends BaseYinYang with SlickConstYinYang with YYSlickCake {
  def stagingAnalyze(): List[scala.Int] = Nil

  def main(): Any
}

trait SlickConstYinYang extends BaseYinYang { self: YYSlickCake =>
  import scala.slick.driver.JdbcDriver.Implicit._

  implicit object LiftInt extends LiftEvidence[scala.Int, Int] {
    def lift(v: scala.Int): Int = YYConstColumn(v)
    def hole(tpe: Manifest[Any], symbolId: scala.Int): Int = null
  }
  implicit object LiftDouble extends LiftEvidence[scala.Double, Double] {
    def lift(v: scala.Double): Double = YYConstColumn(v)
    def hole(tpe: Manifest[Any], symbolId: scala.Int): Double = null
  }
  implicit object LiftLong extends LiftEvidence[scala.Long, Long] {
    def lift(v: scala.Long): Long = YYConstColumn(v)
    def hole(tpe: Manifest[Any], symbolId: scala.Int): Long = null
  }
  implicit object LiftString extends LiftEvidence[Predef.String, String] {
    def lift(v: Predef.String): String = YYConstColumn(v)
    def hole(tpe: Manifest[Any], symbolId: scala.Int): String = null
  }
}