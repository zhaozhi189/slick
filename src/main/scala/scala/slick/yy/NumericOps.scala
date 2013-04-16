package scala.slick.yy

import scala.slick.ast.{ Node, StaticType }
import scala.slick.ast.Library
import scala.slick.ast.LiteralNode

trait NumericOps {
  import StaticType.{ Int => SInt }
  type NumInt = IntOps
  trait IntOps extends Node {
    def +(that: NumInt): Node = Library.+.typed(SInt, this, that)
  }
  object NumInt {
    def apply(i: Int): LiteralNode with NumInt = {
      new LiteralNode with NumInt {
        val value = i
        val tpe = SInt
        def volatileHint = false
        def nodeRebuild = apply(i)
        override def toString = s"LiteralNode $value (volatileHint=$volatileHint)"
      }
    }
  }
}