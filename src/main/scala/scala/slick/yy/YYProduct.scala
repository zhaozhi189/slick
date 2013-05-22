package scala.slick.yy

import scala.slick.lifted.Column
import scala.slick.lifted.Rep
import scala.slick.lifted.Projection
import scala.slick.lifted.Projection2
import scala.slick.lifted.LiftedTuple2

sealed trait YYProjection[T <: Product] extends YYRep[T] with Product {
  def canEqual(that: Any): Boolean = that.isInstanceOf[YYProjection[_]]
}

object YYProjection {
  // TODO generalize it for TupleN
  def fromLiftedProjection[T <: Product](rep: Projection[T]): YYProjection[T] = {
    rep match {
      case tup2: Projection2[_, _] => YYProjection(tup2)
      //      case tup: LiftedTuple2[_, _] => YYTuple(tup)
    }
  }

  // TODO generalize it for TupleN
  def apply[T1, T2](tuple2: Projection2[T1, T2]): YYProjection2[T1, T2] = {
    new YYProjection2[T1, T2] {
      def _1 = YYColumn(tuple2._1)
      def _2 = YYColumn(tuple2._2)
      //      override def underlying = _1.underlying ~ _2.underlying
      override def underlying = tuple2
    }
  }

  // TODO generalize it for TupleN
  def apply[T1, T2](_1: Column[T1], _2: Column[T2]): YYProjection2[T1, T2] = {
    apply(_1 ~ _2)
  }

  // TODO generalize it for TupleN
  def fromYY[T1, T2](_1: YYColumn[T1], _2: YYColumn[T2]): YYProjection2[T1, T2] = {
    apply(_1.underlying ~ _2.underlying)
  }
}

//trait YYProjection2[T1, T2] extends Product2[YYColumn[T1], YYColumn[T2]] with YYProjection[(T1, T2)] {
// TODO generalize it for TupleN
trait YYProjection2[T1, T2] extends Product2[YYColumn[T1], YYColumn[T2]] with YYTuple2[T1, T2] {
  override def toString = "YY(" + _1 + ", " + _2 + ")"
}

object YYTuple {
  // TODO generalize it for TupleN
  def fromMixedTuple[T <: Product](mixed: T): YYProjection[T] = {
    val result = mixed match {
      case (_1, _2) => apply(_1.asInstanceOf[Rep[_]], _2.asInstanceOf[Rep[_]])
    }
    result.asInstanceOf[YYProjection[T]]
  }

  // TODO generalize it for TupleN
  def apply[T1, T2](tuple2: LiftedTuple2[T1, T2]): YYTuple2[T1, T2] = {
    new YYTuple2[T1, T2] {
      def _1 = YYValue.applyUntyped(tuple2._1)
      def _2 = YYValue.applyUntyped(tuple2._2)
      override def underlying = tuple2
    }
  }

  // TODO generalize it for TupleN
  def apply[T1, T2](__1: Rep[T1], __2: Rep[T2]): YYTuple2[T1, T2] = {
    //    new YYTuple2[T1, T2] {
    //      def _1 = YYValue.applyUntyped(__1)
    //      def _2 = YYValue.applyUntyped(__2)
    //      override def underlying = new LiftedTuple2(__1, __2)
    //    }
    apply(new LiftedTuple2(__1, __2))
  }

  // TODO generalize it for TupleN
  def fromYY[T1, T2](_1: YYRep[T1], _2: YYRep[T2]): YYTuple2[T1, T2] = {
    apply(_1.underlying, _2.underlying)
  }
}

// TODO generalize it for TupleN
trait YYTuple2[T1, T2] extends Product2[YYRep[T1], YYRep[T2]] with YYProjection[(T1, T2)] {
  override def toString = "YY(" + _1 + ", " + _2 + ")"
}
//
//object YYProduct {
//  def apply[T <: Product](rep: T): YYProduct[T] = new YYProduct(LiftedProduct(rep))
//}

trait YYOrderingTuples {
  import scala.slick.lifted.{ Ordered => LOrdered }

  // FIXME generalize to TupleN
  def Tuple2[T1, T2](ord1: YYOrdering[T1], ord2: YYOrdering[T2]): YYOrdering[(T1, T2)] =
    new YYOrdering(Ordering.Tuple2(ord1.ord, ord2.ord)) {
      override def toOrdered(x: Rep[(T1, T2)]): LOrdered = yyOrderingListToOrdered(x)((ord1, ord2))
    }
}
