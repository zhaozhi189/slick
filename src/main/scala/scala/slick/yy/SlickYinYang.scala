package scala.slick.yy

import ch.epfl.lamp.yinyang.api.{ BaseYinYang, Interpreted }
import scala.slick.yy.{ Shallow => OShallow }

trait SlickYinYang extends scala.slick.driver.JdbcDriver.ImplicitJdbcTypes with BaseYinYang with SlickConstYinYang with YYSlickCake with Interpreted with TransferCake {
  def stagingAnalyze(allHoles: List[scala.Int]): List[scala.Int] = allHoles
  def reset() = ()
  def interpret[T: Manifest](params: Any*): T = {
    import scala.reflect.runtime.{ universe => ru }
    val result = main()
    val ttag = ru.manifestToTypeTag(ru.runtimeMirror(getClass.getClassLoader), implicitly[Manifest[T]]).asInstanceOf[ru.TypeTag[T]]
    val resType = ttag.tpe.asInstanceOf[ru.TypeRef]
    val queryType = ru.typeOf[OShallow.Query[_]]
    val newRes =
      if (resType <:< queryType)
        new TransferQuery(result.asInstanceOf[YYQuery[_]])
      else
        result
    newRes.asInstanceOf[T]
  }
  def main(): Any
}

trait TransferCake { self: SlickYinYang =>
  class TransferQuery[T](val underlying: YYQuery[T]) extends OShallow.Query[T]
}

trait SlickConstYinYang extends scala.slick.driver.JdbcDriver.ImplicitJdbcTypes { self: SlickYinYang =>
  import scala.slick.ast.TypedType

  implicit object LiftUnit extends LiftEvidence[Unit, Unit] {
    def lift(v: Unit): Unit = v
    def hole(tpe: Manifest[Any], symbolId: scala.Int): Unit = ()
  }
  implicit def LiftConst[T, S](implicit cstTpe: YYConstantType[T, S], manifest: Manifest[T], tpe: TypedType[T]): LiftEvidence[T, S] = new LiftEvidence[T, S] {
    def lift(v: T): S = YYConstColumn(v).asInstanceOf[S]
    def hole(tpe: Manifest[Any], symbolId: scala.Int): S = ???
  }
  implicit def liftQuery[T](implicit manifest: Manifest[OShallow.Query[T]]): LiftEvidence[OShallow.Query[T], Query[T]] = new LiftEvidence[OShallow.Query[T], Query[T]] {
    def lift(v: OShallow.Query[T]): Query[T] = v.asInstanceOf[TransferQuery[T]].underlying
    def hole(tpe: Manifest[Any], symbolId: scala.Int): Query[T] = null
  }
}
