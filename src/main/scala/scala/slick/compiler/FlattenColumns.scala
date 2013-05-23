package scala.slick.compiler

import scala.collection.mutable.HashMap
import scala.slick.ast._
import Util._
import TypeUtil._

/** Replace all TableNodes with TableExpansions which contain both the
  * expansion and the original table. */
class ExpandTables extends Phase {
  val name = "expandTables"

  def apply(state: CompilerState): CompilerState = state.map { n =>
    ClientSideOp.mapServerSide(n)(ch => apply(ch, state))
  }

  def apply(n: Node, state: CompilerState): Node = n match {
    case t: TableExpansion => t
    case t: TableNode =>
      val sym = new AnonSymbol
      val expanded = WithOp.encodeRef(t, sym).nodeTableProjection
      val processed = apply(state.compiler.runBefore(this, state.withNode(expanded)).tree, state)
      TableExpansion(sym, t, ProductNode(processed.flattenProduct))
    case n => n.nodeMapChildren(ch => apply(ch, state))
  }
}

/** Flatten projections and expand multi-column Refs */
class FlattenColumns extends Phase {
  val name = "flattenColumns"

  /** A reference to a flattened type. */
  final case class ElementType(elements: Vector[AnonSymbol], expansion: Type) extends Type {
    def mapChildren(f: Type => Type): Type = {
      val e2 = f(expansion)
      if(e2 == expansion) this else copy(expansion = e2)
    }
    override def select(sym: Symbol): ElementType =
      expansion.select(sym).asInstanceOf[ElementType]
    def expandAround(base: Node): Node = expansion match {
      case ProductType(ch) =>
        ProductNode(ch.map(t => t.asInstanceOf[ElementType].expandAround(base)))
          .nodeWithComputedType(SymbolScope.empty, false, false)
      case t =>
        Select(base, elements.head).nodeTyped(t)
    }
  }

  def apply(state: CompilerState): CompilerState = state.map { n =>
    ClientSideOp.mapServerSide(n)(apply)
  }

  def apply(n: Node): Node = {
    val translated = new HashMap[TypeSymbol, (ElementType, StructType)]
    val tableExpansions = new HashMap[TypeSymbol, Node => Node]
    def flatten(n: Node): (ElementType, StructNode) = (n.nodeType, n) match {
      case (_: ProductType, ProductNode(ns)) =>
        val ch = ns.map(flatten)
        val elts: Vector[ElementType] = ch.map(_._1)(collection.breakOut)
        val tpe = ElementType(elts.flatMap(_.elements), ProductType(elts))
        val struct = StructNode(ch.flatMap{ case(_, StructNode(els)) => els }(collection.breakOut))
        (tpe, struct)
      case (ProductType(types), n) =>
        val prod = ProductNode((1 to types.length).map(idx =>
          tr(select(n, ElementSymbol(idx)).nodeWithComputedType())
        )).nodeWithComputedType()
        flatten(prod)
      case (tpe, n) =>
        val sym = new AnonSymbol
        (ElementType(Vector(sym), tpe), StructNode(Vector(sym -> n)))
    }
    def tr(n: Node): Node = n match {
      case NodeType(Path(_), NominalType(ts)) if translated contains ts =>
        val (repl, structT) = translated(ts)
        val r = repl.expandAround(n.nodeRebuildWithType(NominalType(ts)(structT)))
        logger.debug(s"Replacing translated path $n with:", r)
        r
      case NodeType(_, NominalType(ts)) if tableExpansions contains ts =>
        val r = tableExpansions(ts)(n)
        logger.debug(s"Replacing table path $n with:", r)
        r
      case Select(_: Ref, (_: FieldSymbol | _: AnonSymbol)) => n
      case Select(in, sym) =>
        val r = select(tr(in), sym).nodeWithComputedType()
        logger.debug(s"Replacing select $n with:", r)
        r
      case NodeType(Bind(gen, from, pure @ Pure(p)), CollectionType(cons, NominalType(ts))) =>
        logger.debug(s"Flattening Bind($gen, _, Pure(...)):", p)
        val from2 = tr(from)
        logger.debug(s"  Translated from $gen: ", from2)
        val p2 = tr(p)
        logger.debug(s"  Translated pure value: ", p2)
        val (repl, struct) = flatten(p2)
        val struct2 = struct.nodeWithComputedType()
        logger.debug("  ElementType: "+repl)
        logger.debug("  Struct:", struct2)
        //val struct3 = tr(struct2)
        //logger.debug("  Struct (translated):", struct3)
        translated += ts -> (repl, struct2.nodeType.asInstanceOf[StructType])
        val r = Bind(gen, from2, pure.copy(struct2)).nodeWithComputedType()
        logger.debug(s"Replacing Bind($gen, _, Pure(...)) with:", r)
        r
      case NodeType(TableExpansion(gen, table, columns), CollectionType(_, NominalType(ts))) =>
        logger.debug("Storing expansion for "+ts)
        tableExpansions += ts -> { n: Node => columns.replace({ case Ref(s) if s == gen => n }, keepType = true) }
        table
      case n =>
        val n2 = n.nodeMapChildren(tr).nodeWithComputedType()
        n2.nodeTypedOrCopy(n2.nodeType.replace {
          case t @ NominalType(ts) if translated contains ts =>
            t.withStructuralView(translated(ts)._2)
        })
    }
    tr(n)
  }

  def select(in: Node, sym: Symbol): Node = {
    logger.debug(s"Selecting $sym in:", in)
    (in, sym) match {
      case (StructNode(ch), (s: AnonSymbol)) => ch.find{ case (s2,_) => s == s2 }.get._2
      case (ProductNode(ch), (s: ElementSymbol)) => ch(s.idx-1)
      case (n, s) => Select(n, s)
    }
  }
}
