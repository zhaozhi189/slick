package scala.slick.yy

import scala.slick.ast.Node
import scala.slick.ast.ProductNode
import scala.slick.ast.ProductType
import scala.slick.ast.SymbolScope
import scala.slick.ast.TypedNode

trait TupleOps {
  final class YYTuple2[T1, T2](override val _1: TypedNode, override val _2: TypedNode) extends Tuple2(_1, _2) with YYProjectionOld[(T1, T2)]
}

sealed trait YYProjectionOld[T <: Product] extends ProductNode with Product with TypedNode {
  val nodeChildren = productIterator.map(Node(_)).toIndexedSeq

  override def toString = "YYProjection" + productArity

  lazy val tpe = ProductType(productIterator.asInstanceOf[Iterator[TypedNode]].map(_.tpe).toIndexedSeq)

  override def nodeWithComputedType(scope: SymbolScope, retype: Boolean): Self = if (nodeHasType && !retype) this else {
    val this2 = nodeMapChildren(_.nodeWithComputedType(scope, retype))
    nodeBuildTypedNode(this2, tpe)
  }
}