package scala.slick.yy

import ch.epfl.lamp.yinyang.api.BaseYinYang
import ch.epfl.lamp.yinyang.api.Interpreted
import scala.annotation.tailrec
import scala.slick.SlickException
import _root_.scala.slick.yy.{ Shallow => OShallow }

trait SlickYinYang extends scala.slick.driver.JdbcDriver.ImplicitJdbcTypes with BaseYinYang with SlickConstYinYang with YYSlickCake with Interpreted with TransferCake {
  def stagingAnalyze(allHoles: List[scala.Int]): List[scala.Int] = allHoles

  def reset() = ()
  def interpret[T: Manifest](params: Any*): T = {
    import scala.reflect.runtime.{ universe => ru }
    val m = ru.runtimeMirror(getClass.getClassLoader)

    val result = main()

    //    def convertResultElementToSeq(elem: Any): Seq[Any] =
    //      elem.asInstanceOf[Product].productIterator.toSeq
    //
    //    def convertResultElementToNewElement(tpe: ru.Type)(elem: Any): Any = {
    //      val cm = try {
    //        m.reflectModule(tpe.typeSymbol.companionSymbol.asModule)
    //      } catch {
    //        case _: ScalaReflectionException => {
    //          //          val moduleSymbol = tpe.typeSymbol.companionSymbol.asModule
    //          //
    //          //          @tailrec
    //          //          def findStaticOwner(symbol: ru.Symbol): ru.Symbol = {
    //          //            val owner = symbol.owner
    //          //            if (owner.isStatic)
    //          //              owner
    //          //            else
    //          //              findStaticOwner(owner)
    //          //          }
    //          //
    //          //          val owner = findStaticOwner(moduleSymbol)
    //          //
    //          //          val cls = m.reflectClass(owner.typeSignature.typeSymbol.asClass)
    //          //          val ctor = cls.reflectConstructor(owner.typeSignature.declaration(ru.nme.CONSTRUCTOR).asMethod)
    //          //          val mm = m.reflect(ctor())
    //          //          mm.reflectModule(moduleSymbol)
    //          throw new SlickException("Nested case class in an object is not supported!")
    //        }
    //      }
    //      val app = cm.symbol.asModule.typeSignature.declaration(ru.newTermName("apply")).asMethod
    //      val mm = m.reflect(cm.instance)
    //      val appRef = mm.reflectMethod(app)
    //      val res = appRef(convertResultElementToSeq(elem): _*)
    //      res
    //    }

    val manifest = implicitly[Manifest[T]]
    val ttag = ru.manifestToTypeTag(m, manifest).asInstanceOf[ru.TypeTag[T]]

    val resType = ttag.tpe.asInstanceOf[ru.TypeRef]
    val seqType = ru.typeOf[Seq[_]]
    val queryType = ru.typeOf[_root_.scala.slick.yy.Shallow.Query[_]]

    val newRes =
      if (resType <:< seqType) {
        val tpe = resType.args.head
        //        if (tpe.typeSymbol.asClass.isCaseClass)
        //          result.asInstanceOf[Seq[_]] map convertResultElementToNewElement(tpe)
        //        else
        result
      } else if (resType <:< queryType) {
        new TransferQuery(result.asInstanceOf[YYQuery[_]])
      } else {
        //        if (resType.typeSymbol.asClass.isCaseClass)
        //          convertResultElementToNewElement(resType)(result)
        //        else
        result
      }
    newRes.asInstanceOf[T]
  }

  def main(): Any
}

trait TransferCake { self: SlickYinYang =>
  import scala.slick.driver.JdbcProfile
  import scala.slick.jdbc.JdbcBackend

  class TransferQuery[T](val underlying: YYQuery[T]) extends OShallow.Query[T] {
    //    def first(implicit driver: JdbcProfile, session: JdbcBackend#Session): T =
    //      underlying.first(driver, session)
    //    def toSeq(implicit driver: JdbcProfile, session: JdbcBackend#Session): Seq[T] =
    //      underlying.toSeq(driver, session)
  }

  class QueryElem[T, S]

  implicit def queryElemAnyVal[T <: AnyVal]: QueryElem[T, T] = new QueryElem[T, T]
  implicit def queryElemCaseClass[T <: Product with Serializable]: QueryElem[T, T] = new QueryElem[T, T]
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

  implicit def liftQuery[T, S](implicit manifest: Manifest[OShallow.Query[T]], queryElem: QueryElem[T, S]): LiftEvidence[OShallow.Query[T], Query[S]] = new LiftEvidence[OShallow.Query[T], Query[S]] {
    def lift(v: OShallow.Query[T]): Query[S] = v.asInstanceOf[TransferQuery[S]].underlying
    def hole(tpe: Manifest[Any], symbolId: scala.Int): Query[S] = null
  }

  //  implicit object LiftQuery extends LiftEvidence[OShallow.Query[scala.Int], Query[scala.Int]] {
  //    def lift(v: OShallow.Query[scala.Int]): Query[scala.Int] = v.asInstanceOf[TransferQuery[scala.Int]].underlying
  //    def hole(tpe: Manifest[Any], symbolId: scala.Int): Query[scala.Int] = null
  //  }
}
