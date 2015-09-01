package slick.ast

import Util._
import slick.SlickException
import slick.util.{SlickLogger, DumpInfo, Dumpable, ConstArray}
import scala.collection.{mutable, immutable}
import scala.util.DynamicVariable

/** A symbol which can be used in the AST. It can be either a TypeSymbol or a TermSymbol. */
sealed trait Symbol {
  def name: String
  override def toString = SymbolNamer(this)
}

/** The symbol of a nominal type */
trait TypeSymbol extends Symbol

/** A symbol representing a variable */
trait TermSymbol extends Symbol

/** A named symbol which refers to an (aliased or unaliased) field. */
case class FieldSymbol(name: String)(val options: Seq[ColumnOption[_]], val tpe: Type) extends TermSymbol

/** An element of a ProductNode (using a 1-based index) */
case class ElementSymbol(idx: Int) extends TermSymbol {
  def name = "_" + idx
}

/** A TypeSymbol which uniquely identifies a table type */
trait TableIdentitySymbol extends TypeSymbol

/** Default implementation of TableIdentitySymbol */
case class SimpleTableIdentitySymbol(constituents: AnyRef*) extends TableIdentitySymbol {
  def name = constituents.mkString("@(", ".", ")")
}

/** An anonymous symbol defined in the AST. */
class AnonTypeSymbol extends TypeSymbol {
  def name = "$@"+System.identityHashCode(this)
}

/** An anonymous TableIdentitySymbol. */
class AnonTableIdentitySymbol extends AnonTypeSymbol with TableIdentitySymbol {
  override def toString = "@"+SymbolNamer(this)+""
}

/** An anonymous symbol defined in the AST. */
class AnonSymbol extends TermSymbol {
  def name = "@"+System.identityHashCode(this)
}

/** A Node which introduces TermSymbols. */
trait DefNode extends Node {
  def generators: ConstArray[(TermSymbol, Node)]
  protected[this] def rebuildWithSymbols(gen: ConstArray[TermSymbol]): Node

  final def mapScopedChildren(f: (Option[TermSymbol], Node) => Node): Self with DefNode = {
    val gens = generators
    val ch = children
    val all = ch.zipWithIndex.map[(Option[TermSymbol], Node)] { case (ch, idx) =>
      val o = if(idx < gens.length) Some(gens(idx)._1) else None
      (o, ch)
    }
    val mapped = all.map(f.tupled)
    if(ch.zip(mapped).force.exists { case (n1, n2) => n1 ne n2 }) rebuild(mapped).asInstanceOf[Self with DefNode]
    else this
  }
  final def mapSymbols(f: TermSymbol => TermSymbol): Node = {
    val s = generators.map(_._1)
    val s2 = s.endoMap(f)
    if(s2 eq s) this else rebuildWithSymbols(s2)
  }
}

/** A Node which introduces a TypeSymbol. */
trait TypeGenerator {
  def identity: TypeSymbol
}

/** Provides names for symbols */
class SymbolNamer(treeSymbolPrefix: String, typeSymbolPrefix: String, parent: Option[SymbolNamer] = None) {
  private var curSymbolId = 1
  private val map = new mutable.HashMap[Symbol, String]

  def create(prefix: String) = {
    curSymbolId += 1
    prefix + curSymbolId
  }

  def get(s: Symbol): Option[String] =
    map.get(s) orElse parent.flatMap(_.get(s))

  def apply(s: Symbol): String = get(s).getOrElse(s match {
    case a: AnonSymbol =>
      val n = create(treeSymbolPrefix)
      update(a, n)
      n
    case a: AnonTypeSymbol =>
      val n = create(typeSymbolPrefix)
      update(a, n)
      n
    case s => namedSymbolName(s)
  })

  def namedSymbolName(s: Symbol) = s.name

  def update(s: Symbol, n: String): Unit = map += s -> n

  def use[T](f: => T): T = SymbolNamer.dyn.withValue(this)(f)
}

object SymbolNamer {
  private val dyn = new DynamicVariable[SymbolNamer](null)
  def apply(s: Symbol): String = {
    val n = dyn.value
    if(n eq null) s.name else n(s)
  }
}

/** An immutable symbol table that can resolve a Type by TermSymbol (for local definitions). */
class SymbolScope(map: Map[TermSymbol, Type]) extends Dumpable {
  def + (st: (TermSymbol, Type)): SymbolScope = new SymbolScope(map + st)
  def ++ (t: Traversable[(TermSymbol, Type)]): SymbolScope = new SymbolScope(map ++ t)
  def apply(s: TermSymbol): Type = map(s)
  def get(s: TermSymbol): Option[Type] = map.get(s)
  def getDumpInfo: DumpInfo = DumpInfo("SymbolScope", children =
    map.toSeq.map { case (ts, t) => (ts.toString, new DumpInfo(t.toString)) }.sortBy(_._1))
}

object SymbolScope {
  /** Create a SymbolScope with the given definitions */
  def apply(ts: (TermSymbol, Type)*) = new SymbolScope(ts.toMap)

  /** An empty SymbolScope to which more definitions can be added */
  val empty: SymbolScope = new SymbolScope(Map.empty) {
    override def getDumpInfo = super.getDumpInfo.copy(name = "SymbolScope.empty")
  }
}

/** A mutable symbol table for global type definitions. */
class GlobalTypes private (private var map: mutable.HashMap[TypeSymbol, Type]) extends Dumpable {
  private var frozen = false
  private[this] var cow = true

  private[this] def writableMap: mutable.HashMap[TypeSymbol, Type] = {
    if(frozen) throw new SlickException("Frozen GlobalTypes cannot be modified")
    if(cow) { map = map.clone(); cow = false }
    map
  }

  def += (st: (TypeSymbol, Type)): Unit = writableMap += st
  def ++= (t: Traversable[(TypeSymbol, Type)]): Unit = writableMap ++= t
  def -= (s: TypeSymbol): Unit = {
    GlobalTypes.logger.debug("Removing global TypeSymbol: "+s)
    writableMap -= s
  }
  def --= (t: Traversable[TypeSymbol]): Unit = {
    GlobalTypes.logger.debug("Removing global TypeSymbols: "+t.mkString(", "))
    writableMap --= t
  }
  def apply(s: TypeSymbol): Type = map(s)
  def get(s: TypeSymbol): Option[Type] = map.get(s)
  def symbols: collection.Set[TypeSymbol] = map.keySet

  def getDumpInfo: DumpInfo =
    DumpInfo("GlobalTypes", children = map.toSeq
      .map { case (ts, t) => (ts.toString, new DumpInfo(t.toString)) }.sortBy(_._1))

  /** Freeze this object (disallowing any future modification) and return a new, mutable
    * GlobalTypes instance with all its definitions. */
  def freeze: GlobalTypes = { frozen = true; new GlobalTypes(map) }

  /** Return a frozen snapshot of this object. */
  def snapshot: GlobalTypes = {
    cow = true
    val g = new GlobalTypes(map)
    g.frozen = true
    g
  }

  override def hashCode() = map.hashCode()

  override def equals(o: Any) = o match {
    case o: GlobalTypes => map == o.map
    case _ => false
  }
}

object GlobalTypes {
  private lazy val logger = SlickLogger[GlobalTypes]
  private[this] def emptyMap = new mutable.HashMap[TypeSymbol, Type]

  /** Create an empty GlobalTypes to which more definitions can be added */
  def empty: GlobalTypes = new GlobalTypes(emptyMap)

  /** An immutable empty GlobalTypes which throws an Exception when you try to add or resolve
    * a symbol. This can be used for isolated local type-checking where you know that global
    * types are not needed. */
  val local: GlobalTypes = new GlobalTypes(emptyMap) {
    private[this] def fail = throw new SlickException("GlobalTypes.local")
    override def += (st: (TypeSymbol, Type)): Unit = fail
    override def ++= (t: Traversable[(TypeSymbol, Type)]): Unit = fail
    override def apply(s: TypeSymbol): Type = fail
    override def get(s: TypeSymbol): Option[Type] = fail
    override def getDumpInfo: DumpInfo = super.getDumpInfo.copy(name = "GlobalTypes.local")
  }
}
