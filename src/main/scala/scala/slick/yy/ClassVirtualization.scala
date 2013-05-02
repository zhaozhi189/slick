package scala.slick.yy

import scala.reflect.runtime.{ universe => runtimeUniverse }
import runtimeUniverse._
import Flag.CASE
import scala.slick.schema.Table
import scala.slick.schema.Column
import scala.slick.schema.QualifiedName
import scala.slick.typeproviders.MacroHelpers
import scala.slick.typeproviders.DefaultContextUtils

object ClassVirtualization extends (Tree => Tree) {
  def apply(tree: Tree): Tree = new ClassVirtualization().transform(tree)
}

class ClassVirtualization extends Transformer {
  val mirror = runtimeUniverse.runtimeMirror(getClass.getClassLoader())
  val macroHelper = new {
    val universe: runtimeUniverse.type = runtimeUniverse
  } with MacroHelpers(DefaultContextUtils, "")
  // workaround for compatibility of 2.10 and macro paradise
  object TermName {
    def apply(s: String): TermName = newTermName(s)
    def unapply(t: TermName): Option[String] = Some(t.toString)
  }
  object TypeName {
    def apply(s: String): TypeName = newTypeName(s)
    def unapply(t: TypeName): Option[String] = Some(t.toString)
  }

  def isCaseClassDef(tree: Tree): Boolean = tree match {
    case ClassDef(mods, _, _, _) if mods.hasFlag(CASE) => true
    case _ => false
  }

  def convertCaseClass(tree: Tree) = {
    tree match {
      case ClassDef(mods, TypeName(tName), tparams, impl @ Template(parents: List[Tree], self: ValDef, body: List[Tree])) if isCaseClassDef(tree) => {
        def annotationToName(tree: Tree): Option[String] =
          tree match {
            case Apply(_, List(Literal(Constant(tName: String)))) => Some(tName)
            case _ => None
          }
        val tableName = mods.annotations.headOption.flatMap(annotationToName).getOrElse(tName.toUpperCase())
        val tableQName = QualifiedName.tableName(tableName)
        val columns = body.collect({
          case ValDef(mods, TermName(cName), tpt: Tree, rhs: Tree) => {
            val columnName = mods.annotations.headOption.flatMap(annotationToName).getOrElse(cName.toUpperCase())
            val columnQName = QualifiedName.columnName(tableQName, columnName)
            val tpe = mirror.staticClass("scala." + tpt.toString).toType
            Column(columnQName, tpe, cName, cName)
          }
        })
        val table = Table(tableQName, columns, Nil, tName, tName + "Row")
        println(table)
        val caseClassDef = macroHelper.tableToCaseClass(table) match {
          case ClassDef(mods, name, tparams, Template(parents, self, body)) => {
            val trTree = macroHelper.typeToTree(typeOf[YYTableRow])
            ClassDef(mods, name, tparams, Template(trTree :: parents, self, body))
          }
        }
        val moduleDef = macroHelper.tableToModule(table)
        List(caseClassDef, moduleDef)
      }
      case _ => Nil
    }
  }

  override def transform(tree: Tree): Tree = tree match {
    case Block(stats, expr) if (stats.exists(isCaseClassDef(_))) => {
      //      val (caseClasses, others) = stats.partition(isCaseClassDef(_))
      //      val newClasses = caseClasses flatMap convertCaseClass
      //      Block(newClasses ++ others.map(transform(_)), transform(expr))
      Block(stats.flatMap(x => if (isCaseClassDef(x)) convertCaseClass(x) else List(transform(x))), transform(expr))
    }
    case _ => super.transform(tree)
  }
}

import scala.annotation.StaticAnnotation
final case class Entity(name: String) extends StaticAnnotation