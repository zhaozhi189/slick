package scala.slick.yy

import ch.epfl.lamp.yinyang.api.BaseYinYang
import ch.epfl.lamp.yinyang.api.Interpreted

trait SlickYinYang extends scala.slick.driver.JdbcDriver.ImplicitJdbcTypes with BaseYinYang with SlickConstYinYang with YYSlickCake with Interpreted {
  def stagingAnalyze(allHoles: List[scala.Int]): List[scala.Int] = allHoles

  def reset() = ()
  def interpret[T: Manifest](params: Any*): T = {
    main().asInstanceOf[T]
  }

  def main(): Any
}

//trait ImplicitSlickYinYang extends SlickYinYang {
//  type Session = scala.slick.jdbc.JdbcBackend#Session
//  def session: Session
//
//  override def interpret[T: Manifest](params: Any*): T = {
//    main().asInstanceOf[(scala.slick.jdbc.JdbcBackend#Session) => T](session)
//  }
//}

trait SlickConstYinYang extends scala.slick.driver.JdbcDriver.ImplicitJdbcTypes with BaseYinYang { self: SlickYinYang =>

  implicit object LiftUnit extends LiftEvidence[Unit, Unit] {
    def lift(v: Unit): Unit = v
    def hole(tpe: Manifest[Any], symbolId: scala.Int): Unit = ()
  }
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
  implicit object LiftBoolean extends LiftEvidence[scala.Boolean, Boolean] {
    def lift(v: scala.Boolean): Boolean = YYConstColumn(v)
    def hole(tpe: Manifest[Any], symbolId: scala.Int): Boolean = null
  }
  implicit object LiftString extends LiftEvidence[Predef.String, String] {
    def lift(v: Predef.String): String = YYConstColumn(v)
    def hole(tpe: Manifest[Any], symbolId: scala.Int): String = null
  }
}
