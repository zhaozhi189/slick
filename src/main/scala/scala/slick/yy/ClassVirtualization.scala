package scala.slick.yy

import scala.slick.schema.Table
import scala.slick.schema.Column
import scala.slick.schema.QualifiedName
import scala.slick.typeproviders.MacroHelpers
import scala.slick.typeproviders.DefaultContextUtils
import scala.reflect.api.Universe
import scala.reflect.api.Mirror

trait YYTransformers {
  val universe: Universe
  val mirror: universe.Mirror

  import universe._
  import Flag.CASE
  import Flag.PARAM

  val macroHelper = new {
    val universe: YYTransformers.this.universe.type = YYTransformers.this.universe
  } with MacroHelpers(DefaultContextUtils, "")

  object ClassVirtualization extends (Tree => Tree) {
    def apply(tree: Tree): Tree = {
      println(tree)
      val res = new ClassVirtualization().transform(tree)
      println(res)
      res
    }
  }

  class ClassVirtualization extends Transformer {
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

    val SYNTHETIC = scala.reflect.internal.Flags.SYNTHETIC.asInstanceOf[Long].asInstanceOf[FlagSet]

    def isCaseClassObject(tree: Tree): Boolean = tree match {
      case ModuleDef(mods, _, _) if mods.hasFlag(SYNTHETIC) => true
      case _ => false
    }

    def convertCaseClass(tree: Tree) = {
      def createYYTable(table: Table): ClassDef = {
        val yyTableName = "YY" + table.moduleName
        val PARAMACCESSOR = scala.reflect.internal.Flags.PARAMACCESSOR.asInstanceOf[Long].asInstanceOf[FlagSet]
        val typeParamName = TypeName(table.caseClassName)
        val yyTableType = macroHelper.createClassFromString("_root_.scala.slick.yy.YYTable")
        val tableSuper = AppliedTypeTree(yyTableType, List(Ident(typeParamName)))
        val tableParamVar = "table"
        val tableClassTree = macroHelper.createClass(table.moduleName, Nil)
        val tableParam = ValDef(Modifiers(PARAMACCESSOR), TermName(tableParamVar), tableClassTree, EmptyTree)
        val superCall = Apply(Select(Super(This(tpnme.EMPTY), tpnme.EMPTY), nme.CONSTRUCTOR), List())
        val constructorBody = ValDef(Modifiers(PARAM | PARAMACCESSOR), TermName(tableParamVar), tableClassTree, EmptyTree)
        val constructor = DefDef(NoMods, nme.CONSTRUCTOR, List(), List(List(constructorBody)), TypeTree(), Block(List(superCall), Literal(Constant(()))))
        val fields = table.columns map { c =>
          DefDef(NoMods, TermName(c.moduleFieldName), List(), List(), TypeTree(), Apply(Select(macroHelper.createObjectFromString("_root_.scala.slick.yy.YYColumn"), TermName("apply")), List(Select(Select(This(TypeName(yyTableName)), TermName(tableParamVar)), TermName(c.moduleFieldName)))))
        }
        val methods = tableParam :: constructor :: fields
        ClassDef(NoMods, TypeName(yyTableName), List(), Template(List(tableSuper), emptyValDef, methods))
      }

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
            case ValDef(mods, TermName(cNameRaw), tpt: Tree, rhs: Tree) => {
              val cName = cNameRaw.trim // I don't know why, but there's an extra space at the end of value name
              val columnName = mods.annotations.headOption.flatMap(annotationToName).getOrElse(cName.toUpperCase())
              val columnQName = QualifiedName.columnName(tableQName, columnName)
              val tpe = try {
                mirror.staticClass("scala." + tpt.toString).toType
              } catch {
                case e: scala.reflect.internal.MissingRequirementError =>
                  {
                    val columnTypes = Map(
                      "Int" -> typeOf[Int], "Double" -> typeOf[Double], "String" -> typeOf[String],
                      "scala.Int" -> typeOf[Int], "scala.Double" -> typeOf[Double], "scala.String" -> typeOf[String],
                      "java.lang.String" -> typeOf[String], "Boolean" -> typeOf[Boolean], "scala.Boolean" -> typeOf[Boolean])
                    columnTypes(tpt.toString)
                  }
              }
              Column(columnQName, tpe, cName, cName)
            }
          })
          val table = Table(tableQName, columns, Nil, tName, tName + "Row")
          println(table)
          val caseClassDef = macroHelper.tableToCaseClass(table) match {
            case ClassDef(mods, name, tparams, Template(parents, self, body)) => {
              //              val trTree = macroHelper.typeToTree(typeOf[YYTableRow])
              val trTree = macroHelper.createClassFromString("_root_.scala.slick.yy.YYTableRow")
              ClassDef(mods, name, tparams, Template(trTree :: parents, self, body))
            }
          }
          val tableClassDef = macroHelper.tableToModule(table) match {
            case ModuleDef(mods, TermName(name), Template(List(_), self, body)) => {
              val tableType = macroHelper.createClassFromString("_root_.scala.slick.driver.JdbcDriver.simple.Table")
              val typeParamName = TypeName(table.caseClassName)
              val tableSuper = AppliedTypeTree(tableType, List(Ident(typeParamName)))
              ClassDef(mods, TypeName(name), Nil, Template(List(tableSuper), self, body))
            }
          }
          val yyTableClassDef = createYYTable(table)
          //          List(caseClassDef)
          List(caseClassDef, tableClassDef)
          //          List(caseClassDef, tableClassDef, yyTableClassDef)
        }
        case _ => Nil
      }
    }

    override def transform(tree: Tree): Tree = tree match {
      case Block(stats, expr) if (stats.exists(isCaseClassDef(_))) => {
        //      val (caseClasses, others) = stats.partition(isCaseClassDef(_))
        //      val newClasses = caseClasses flatMap convertCaseClass
        //      Block(newClasses ++ others.map(transform(_)), transform(expr))
        //        Block(stats.flatMap(x => if (isCaseClassDef(x)) convertCaseClass(x) else List(transform(x))), transform(expr))
        Block(stats.flatMap(x => if (isCaseClassDef(x)) convertCaseClass(x) else if (isCaseClassObject(x)) Nil else List(transform(x))), transform(expr))
      }
      case _ => super.transform(tree)
    }
  }
}

import scala.annotation.StaticAnnotation
final case class Entity(name: String) extends StaticAnnotation