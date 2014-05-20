package scala.slick.lifted

import scala.slick.ast._
import scala.slick.ast.Util._

object AggWinFuncSupport {
  import scala.language.higherKinds
  import scala.language.implicitConversions

  sealed class Mode(val name: String)
  sealed class RowCursor(val desc: String)

  object FrameElements {
    case object ROWS extends Mode("rows")
    case object RANGE extends Mode("range")

    case object currentRow extends RowCursor("current row")
    case class boundPreceding[T <: AnyVal](value: T) extends RowCursor(s"$value preceding")
    case object unboundPreceding extends RowCursor("unbounded preceding")
    case class boundFollowing[T <: AnyVal](value: T) extends RowCursor(s"$value following")
    case object unboundFollowing extends RowCursor("unbounded following")
  }

  /** A distinct definition expression for aggregate function */
  final case class Distinct(generator: Symbol, from: Node) extends FilteredQuery with DefNode {
    type Self = Distinct
    protected[this] def nodeRebuildWithGenerators(gen: IndexedSeq[Symbol]): Node = copy(generator = gen(0))
    protected[this] def nodeRebuild(ch: IndexedSeq[Node]): Self = copy(from = ch(0))
    def nodeChildren: Seq[Node] = Seq(from)
    override def toString = "Distinct"
  }

  final case class AggFuncInputs(aggParams: Seq[Node], modifier: Option[String] = None, orderBy: Seq[(Node, Ordering)] = Nil) extends SimplyTypedNode {
    type Self = AggFuncInputs
    val nodeChildren = aggParams ++ orderBy.map(_._1)
    protected[this] def nodeRebuild(ch: IndexedSeq[Node]): Self = {
      val newAggParams = ch.slice(0, aggParams.length)
      val orderByOffset = aggParams.length
      val newOrderBy = ch.slice(orderByOffset, orderByOffset + orderBy.length)
      copy(aggParams = newAggParams,
        orderBy = (orderBy, newOrderBy).zipped.map { case ((_, o), n) => (n, o) })
    }
    protected def buildType = aggParams(0).nodeType
    override def toString = "AggFuncInputs"
  }

  /** A frame definition expression for window function */
  final case class WindowFrame(generator: Symbol, from: Node, frameDef: (String, String, Option[String])) extends FilteredQuery with DefNode {
    type Self = WindowFrame
    protected[this] def nodeRebuildWithGenerators(gen: IndexedSeq[Symbol]): Node = copy(generator = gen(0))
    protected[this] def nodeRebuild(ch: IndexedSeq[Node]): Self = copy(from = ch(0))
    def nodeChildren: Seq[Node] = Seq(from)
    override def toString = "FrameDef"
  }

  /** A window function expression; clauses should be: aggExpr [partition by ...] [order by ...] [rows between .. and ..] */
  final case class WindowExpr(aggExpr: Node, partitionBy: Seq[Node], orderBy: Seq[(Node, Ordering)],
                              frameDef: Option[(String, String, Option[String])] = None) extends SimplyTypedNode {
    type Self = WindowExpr
    val nodeChildren = aggExpr +: (partitionBy ++ orderBy.map(_._1))
    protected[this] def nodeRebuild(ch: IndexedSeq[Node]): Self = {
      val newAggExpr = ch(0)
      val partitionByOffset = 1
      val newPartitionBy = ch.slice(partitionByOffset, partitionByOffset + partitionBy.length)
      val orderByOffset = partitionByOffset + partitionBy.length
      val newOrderBy = ch.slice(orderByOffset, orderByOffset + orderBy.length)
      copy(aggExpr = newAggExpr, partitionBy = newPartitionBy,
        orderBy = (orderBy, newOrderBy).zipped.map { case ((_, o), n) => (n, o) })
    }
    protected def buildType = aggExpr.nodeType
    override def toString = "WindowExpr"
  }

  ///
  trait AggregateSupport[TT <: AbstractTable[_]] {
    def theTable: TT
    def mkColumn[T: TypedType](aggFunc: FunctionSymbol, params: List[Node] = Nil): Column[T]
  }

  abstract class AggregateFunctionExtensionMethodsBase[E <: AbstractTable[_], U, C[_]](val q: Query[E, U, C]) extends AggregateSupport[E] {

    override val theTable: E = q.shaped.value

    override def mkColumn[R: TypedType](aggFunc: FunctionSymbol, params: List[Node]): Column[R] = {
      val distinct = q.toNode.findNode(_.isInstanceOf[Distinct]).map(t => "DISTINCT")
      val orderBy = q.toNode.findNode(_.isInstanceOf[SortBy]).map(_.asInstanceOf[SortBy].by).getOrElse(Nil)
      new Column[R] {
        def toNode = aggFunc.typed(tpe, AggFuncInputs(params, distinct, orderBy))
      }
    }

    def distinct(): Query[E, U, C] = {
      val generator = new AnonSymbol
      new WrappingQuery[E, U, C](Distinct(generator, q.toNode), q.shaped)
    }
  }

  abstract class WindowFunctionExtensionMethodsBase[K, T, G, P <: AbstractTable[_], U, C[_]](val q: Query[(G, Query[P, U, C]), (T, Query[P, U, C]), C]) extends AggregateSupport[P] {

    override val theTable: P = {
      var r: P = null.asInstanceOf[P]
      q.map({ case (_, q0) => r = q0.shaped.value })
      r
    }

    override def mkColumn[R: TypedType](aggFunc: FunctionSymbol, params: List[Node] = Nil): Column[R] = {
      val partitionBy = q.toNode.findNode(_.isInstanceOf[GroupBy]).map(_.asInstanceOf[GroupBy].by +: Nil).getOrElse(Nil)
      val orderBy = q.toNode.findNode(_.isInstanceOf[SortBy]).map(_.asInstanceOf[SortBy].by).getOrElse(Nil)
      val frameDef = q.toNode.findNode(_.isInstanceOf[WindowFrame]).map(_.asInstanceOf[WindowFrame].frameDef)
      new Column[R] {
        def toNode = WindowExpr(aggFunc.typed(tpe, params: _*), partitionBy, orderBy, frameDef)
      }
    }

    def withFrame(f: FrameElements.type => (Mode, RowCursor, Option[RowCursor])): Query[(G, Query[P, U, C]), (T, Query[P, U, C]), C] = {
      val generator = new AnonSymbol
      val (mode, start, end) = f(FrameElements)
      val frameDef = WindowFrame(generator, q.toNode, (mode.name, start.desc, end.map(_.desc)))
      new WrappingQuery[(G, Query[P, U, C]), (T, Query[P, U, C]), C](frameDef, q.shaped)
    }
  }
}
