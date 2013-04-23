package scala.slick

import ch.epfl.lamp.yinyang._
import scala.language.experimental.macros
import scala.reflect.macros.Context

package object yy {
  def slickYY[T](block: => T): T = macro implementations.slickYY[T]
  def slickYYDebug[T](block: => T): T = macro implementations.slickYYDebug[T]

  object implementations {
    def slickYY[T](c: Context)(block: c.Expr[T]): c.Expr[T] =
      new YYTransformer[c.type, T](c, "scala.slick.yy.SlickYinYang",
        shallow = false,
        debug = false,
        rep = false)(block)

    def slickYYDebug[T](c: Context)(block: c.Expr[T]): c.Expr[T] =
      new YYTransformer[c.type, T](c, "scala.slick.yy.SlickYinYang",
        shallow = false,
        debug = true,
        rep = false)(block)
  }
}
