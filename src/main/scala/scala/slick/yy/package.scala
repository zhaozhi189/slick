package scala.slick

import ch.epfl.yinyang._
import ch.epfl.yinyang.typetransformers.PolyTransformer
import ch.epfl.yinyang.transformers.PostProcessing
import scala.language.experimental.macros
import scala.language.existentials
import scala.reflect.macros.Context
import scala.slick.jdbc.JdbcBackend
import scala.slick.driver.JdbcDriver

package object yy {
  def shallow[T](block: => T): T = macro implementations.slick[T]
  def shallowDebug[T](block: => T): T = macro implementations.slickDebug[T]

  object implementations {
    def slick[T](c: Context)(block: c.Expr[T]): c.Expr[T] = {
      //      println(c.universe.showRaw(block))
      val yyTranformers = new {
        val universe: c.universe.type = c.universe
        val mirror = c.mirror
      } with YYTransformers
      val virtualSymbols = yyTranformers.VirtualClassCollector(block.tree)
      val virtualTypes = virtualSymbols.map(_.typeSignature)
      val virtualStatements = yyTranformers.ClassVirtualization.getStatementsFromTables

      YYTransformer[c.type, T](c)("scala.slick.yy.SlickYinYang",
        new SlickTypeTransformer[c.type](c)(virtualTypes),
        postProcessing = Some(new PostProcessing[c.type](c)(virtualStatements)),
        Map("shallow" -> false, "debug" -> 0, "featureAnalysing" -> false, "ascriptionTransforming" -> true)
      )(block)
    }
    def slickDebug[T](c: Context)(block: c.Expr[T]): c.Expr[T] = {
      //      println(c.universe.showRaw(block))
      val yyTranformers = new {
        val universe: c.universe.type = c.universe
      } with YYTransformers
      val virtualSymbols = yyTranformers.VirtualClassCollector(block.tree)
      val virtualTypes = virtualSymbols.map(_.typeSignature)
      val virtualStatements = yyTranformers.ClassVirtualization.getStatementsFromTables

      YYTransformer[c.type, T](c)("scala.slick.yy.SlickYinYang",
        new SlickTypeTransformer[c.type](c, 1)(virtualTypes),
        postProcessing = Some(new PostProcessing[c.type](c)(virtualStatements)),
        Map("shallow" -> false, "debug" -> 1, "featureAnalysing" -> false, "ascriptionTransforming" -> true)
      )(block)
    }

  }
}
