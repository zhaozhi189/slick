package scala.slick.yy

import scala.slick.ast.Library
import scala.slick.ast.LiteralNode
import scala.slick.ast.Node
import scala.slick.ast.StaticType
import scala.slick.ast.TypedNode

trait NumericOps {
  import StaticType.{ Int => SInt }
  type NumInt = IntOps
  trait IntOps extends TypedNode {
    val tpe = SInt
    def +(that: NumInt): TypedNode = Library.+.typed(tpe, this, that)
  }
  object NumInt {
    def apply(i: Int): LiteralNode with NumInt = {
      new LiteralNode with NumInt {
        val value = i
//        val tpe = SInt
        def volatileHint = false
        def nodeRebuild = apply(i)
        override def toString = s"LiteralNode $value (volatileHint=$volatileHint)"
      }
    }
  }
}

