package slick.ast

import Util._
import slick.SlickException
import slick.util.ConstArray
import scala.collection.immutable
import scala.collection.mutable.HashMap
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

/** A Node which introduces Symbols. */
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

/** Provides names for symbols */
class SymbolNamer(treeSymbolPrefix: String, typeSymbolPrefix: String, parent: Option[SymbolNamer] = None) {
  private var curSymbolId = 1
  private val map = new HashMap[Symbol, String]

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

/** An immutable symbol table that can resolve a Type by TermSymbol (for local definitions) or
  * TypeSymbol (for global NominalTypes). */
class SymbolScope(val _map: Map[Symbol, Type]) extends AnyVal {
  def + (st: (Symbol, Type)): SymbolScope = new SymbolScope(_map + st)
  def apply(s: Symbol): Type = _map(s)
  def get(s: Symbol): Option[Type] = _map.get(s)
}

object SymbolScope {
  /** The empty SymbolScope to which more definitions can be added */
  val empty: SymbolScope = new SymbolScope(Map.empty)

  /** A special empty SymbolScope that cannot be queried or have TypeSymbols added to it. This is
    * used for inferring types in well isolated parts of an AST where you know (and want to
    * enforce) that no global types need to be used. */
  val local: SymbolScope = new SymbolScope(new immutable.HashMap[Symbol, Type] {
    override def + [B1 >: Type](kv: (Symbol, B1)): immutable.HashMap[Symbol, B1] = kv._1 match {
      case _: TypeSymbol =>
        throw new SlickException("SymbolScope.local cannot add new TypeSymbols")
      case _ => super.+(kv)
    }
    override def apply(s: Symbol): Type =
      throw new SlickException("SymbolScope.local cannot resolve Symbols")
    override def get(s: Symbol): Option[Type] =
      throw new SlickException("SymbolScope.local cannot resolve Symbols")
  })
}
