package scala.slick

import ch.epfl.lamp.yinyang._
import scala.language.experimental.macros
import scala.reflect.macros.Context

package object yy {
  def slickYY[T](block: => T): T = macro implementations.slickYY[T]
  def slickYYDebug[T](block: => T): T = macro implementations.slickYYDebug[T]
  def slickYYV[T](block: => T): T = macro implementations.slickYYV[T]
  def slickYYVP[T](block: => T): T = macro implementations.slickYYVP[T]
  def slickYYVDebug[T](block: => T): T = macro implementations.slickYYVDebug[T]

  object implementations {
    def slickYY[T](c: Context)(block: c.Expr[T]): c.Expr[T] =
      new YYTransformer[c.type, T](c, "scala.slick.yy.SlickYinYang",
        shallow = false,
        debug = false,
        rep = false,
        slickHack = true)(block)

    def slickYYDebug[T](c: Context)(block: c.Expr[T]): c.Expr[T] =
      new YYTransformer[c.type, T](c, "scala.slick.yy.SlickYinYang",
        shallow = false,
        debug = true,
        rep = false,
        slickHack = true)(block)

    def slickYYV[T](c: Context)(block: c.Expr[T]): c.Expr[T] = {
      val ClassVirtualization = {
        new {
          val universe: c.universe.type = c.universe
          val mirror = c.mirror
        } with YYTransformers
      }.ClassVirtualization.asInstanceOf[(Context#Tree => Context#Tree)]

      new YYTransformer[c.type, T](c, "scala.slick.yy.SlickYinYang",
        shallow = false,
        debug = false,
        rep = false,
        slickHack = true,
        preprocess = ClassVirtualization)(block)
    }
    def slickYYVP[T](c: Context)(block: c.Expr[T]): c.Expr[T] = {
      //      println(c.universe.showRaw(block))
      val yyTranformers = new {
        val universe: c.universe.type = c.universe
        val mirror = c.mirror
      } with YYTransformers
      yyTranformers.VirtualClassCollector(block.tree)
      val ClassVirtualization = yyTranformers.ClassVirtualization.asInstanceOf[(Context#Tree => Context#Tree)]

      new YYTransformer[c.type, T](c, "scala.slick.yy.SlickYinYang",
        shallow = false,
        debug = false,
        rep = false,
        slickHack = true,
        preprocess = ClassVirtualization)(block)
    }

    def slickYYVDebug[T](c: Context)(block: c.Expr[T]): c.Expr[T] = {
      val ClassVirtualization = {
        new {
          val universe: c.universe.type = c.universe
          val mirror = c.mirror
        } with YYTransformers
      }.ClassVirtualization.asInstanceOf[(Context#Tree => Context#Tree)]

      new YYTransformer[c.type, T](c, "scala.slick.yy.SlickYinYang",
        shallow = false,
        debug = true,
        rep = false,
        slickHack = true,
        preprocess = ClassVirtualization)(block)
    }
  }
}
