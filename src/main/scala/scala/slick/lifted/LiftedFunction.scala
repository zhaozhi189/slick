package scala.slick.lifted

import scala.slick.ast._
import scala.slick.profile.BasicProfile
import scala.annotation.implicitNotFound

/** An Arrow with application for composition of functions over shaped
  * values. This allows the composition of compiled and non-compiled query
  * functions.
  *
  * It cannot itself be a Rep[S => T] because there is no way to create an
  * instance of an arbitrary packed type S given only its Shape but no
  * prototype value. Such an instance (with a Ref encoded into it) would
  * be needed to create an AST node representing a LiftedFunction. */
trait LiftedFunction[S, SU, T, TU] {
  val f: S => T
  implicit val sshape: Shape[ShapeLevel.All, S, SU, S]
  implicit val tshape: Shape[ShapeLevel.All, T, TU, T]

  def >>> [U, UU](g: LiftedFunction[T, TU, U, UU]): LiftedFunction[S, SU, U, UU] =
    LiftedFunction[S, SU, U, UU](f andThen g.f)(sshape, g.tshape)

  def <<< [U, UU](g: LiftedFunction[U, UU, S, SU]): LiftedFunction[U, UU, T, TU] =
    g >>> this

  def fst[U, UU](implicit ushape: Shape[ShapeLevel.All, U, UU, U]): LiftedFunction[(S, U), (SU, UU), (T, U), (TU, UU)] =
    LiftedFunction[(S, U), (SU, UU), (T, U), (TU, UU)] { case (s, u) => (f(s), u) }

  def snd[U, UU](implicit ushape: Shape[ShapeLevel.All, U, UU, U]): LiftedFunction[(U, S), (UU, SU), (U, T), (UU, TU)] =
    LiftedFunction((u: U, s: S) => (s, u)) >>> (fst[U, UU] >>> LiftedFunction((t: T, u: U) => (u, t)))

  def *** [U, UU, V, VU](g: LiftedFunction[U, UU, V, VU]): LiftedFunction[(S, U), (SU, UU), (T, V), (TU, VU)] =
    fst[U, UU](g.sshape) >>> g.snd

  def &&& [V, VU](g: LiftedFunction[S, SU, V, VU]): LiftedFunction[S, SU, (T, V), (TU, VU)] =
    LiftedFunction[S, SU, (S, S), (SU, SU)](s => (s, s)) >>> (this *** g)

  def compile(implicit driver: BasicProfile, sushape: Shape[ShapeLevel.Columns, SU, SU, S]): CompiledLiftedFunction[S, SU, T, TU] =
    new CompiledLiftedFunction(f, driver)

  /** The bind operation for the arrow monad */
  def flatMap[U, UU](f: T => LiftedFunction[Unit, Unit, U, UU])(implicit ushape: Shape[ShapeLevel.All, U, UU, U]): LiftedFunction[S, SU, U, UU] =
    this >>> (LiftedFunction((t: T) => (f(t), ())) >>> LiftedFunction.app)

  def map[U, UU](f: T => U)(implicit ushape: Shape[ShapeLevel.All, U, UU, U]): LiftedFunction[S, SU, U, UU] =
    this >>> LiftedFunction(f)

  //def apply(v: S): T = f(v)
}

object LiftedFunction {
  /** Lift a pure function into the arrow */
  def apply[S, SU, T, TU](_f: S => T)(implicit _sshape: Shape[ShapeLevel.All, S, SU, S], _tshape: Shape[ShapeLevel.All, T, TU, T]): LiftedFunction[S, SU, T, TU] =
    new LiftedFunction[S, SU, T, TU] {
      val f = _f
      val sshape = _sshape
      val tshape = _tshape
    }

  @inline def apply[S1, S2, SU, T, TU](f: (S1, S2) => T)(implicit _sshape: Shape[ShapeLevel.All, (S1, S2), SU, (S1, S2)], _tshape: Shape[ShapeLevel.All, T, TU, T]): LiftedFunction[(S1, S2), SU, T, TU] =
    apply[(S1, S2), SU, T, TU](f.tupled)

  /** Application for this arrow */
  def app[B, BU, C, CU](implicit lshape: Shape[ShapeLevel.All, LiftedFunction[B, BU, C, CU], B => C, LiftedFunction[B, BU, C, CU]], bshape: Shape[ShapeLevel.All, B, BU, B], cshape: Shape[ShapeLevel.All, C, CU, C]): LiftedFunction[(LiftedFunction[B, BU, C, CU], B), (B => C, BU), C, CU] =
    apply[(LiftedFunction[B, BU, C, CU], B), (B => C, BU), C, CU] { case (f, b) => f.f(b) }

  /** Lift a pure value into the arrow monad */
  def pure[A, AU](v: A)(implicit ashape: Shape[ShapeLevel.All, A, AU, A]): LiftedFunction[Unit, Unit, A, AU] =
    apply[Unit, Unit, A, AU]((_: Unit) => v)
}

class CompiledLiftedFunction[S, SU, T, TU](val f: S => T, val driver: BasicProfile)(implicit val sshape: Shape[ShapeLevel.All, S, SU, S], val tshape: Shape[ShapeLevel.All, T, TU, T], val sushape: Shape[ShapeLevel.Columns, SU, SU, S])
  extends LiftedFunction[S, SU, T, TU] {
  val param = sushape.buildParams(_.asInstanceOf[SU])
  val ast = tshape.toNode(f(param))
}
