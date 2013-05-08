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
      //      println(tree)
      val res = new ClassVirtualization().transform(tree)
      //      println(res)
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
      case ClassDef(mods, _, _, _) => true
      //      case ClassDef(mods, _, _, _) if mods.hasFlag(CASE) => true
      case _ => false
    }

    val SYNTHETIC = scala.reflect.internal.Flags.SYNTHETIC.asInstanceOf[Long].asInstanceOf[FlagSet]

    def isCaseClassObject(tree: Tree): Boolean = tree match {
      case ModuleDef(mods, _, _) if mods.hasFlag(SYNTHETIC) => true
      case _ => false
    }

    def getTableFromCaseClassDef(classDef: ClassDef) = classDef match {
      case ClassDef(mods, TypeName(tName), tparams, impl @ Template(parents: List[Tree], self: ValDef, body: List[Tree])) if isCaseClassDef(classDef) => {
        //    	  def annotationToName(ann: Annotation): Option[String] =
        //    			  ann match {
        //    			  case Apply(_, List(Literal(Constant(tName: String)))) => Some(tName)
        //    			  case _ => None
        //    	  }
        def getNameOfSymbol(symbol: Symbol): Option[String] = {
          // workaround for SI-7424
          symbol.typeSignature
          symbol.annotations.foreach(_.tpe)
//          println(s"Annotations: ${symbol.annotations}")

          symbol.annotations.headOption.map(_.scalaArgs.head).flatMap(annotationToName)
        }

        def annotationToName(tree: Tree): Option[String] =
          tree match {
            //            case Apply(_, List(Literal(Constant(tName: String)))) => Some(tName)
            case Literal(Constant(name: String)) => Some(name)
            case _ => None
          }
        //          println(showRaw(tree))
        val tableName = getNameOfSymbol(classDef.symbol).getOrElse(tName.toUpperCase())
        //        val tableName = mods.annotations.headOption.flatMap(annotationToName).getOrElse(tName.toUpperCase())
        val tableQName = QualifiedName.tableName(tableName)
        //        classDef.symbol.typeSignature.members.foreach(m => {
        //          //          if (m.isParameter) 
        //          {
        //            println(m)
        //            m.typeSignature
        //            m.annotations.foreach(_.tpe)
        //            println(s"Annotations: ${m.annotations}")
        //          }
        //        })
        //        val columns = body.collectFirst({
        //          case DefDef(_, name, _, vparamss, _, _) if name.equals(nme.CONSTRUCTOR) => {
        //            vparamss.head.map {
        //              case valDef @ ValDef(mods, TermName(cNameRaw), tpt: Tree, rhs: Tree) => {
        //                println(valDef)
        //                val sym = valDef.symbol
        //                sym.typeSignature
        //                sym.annotations.foreach(_.tpe)
        //                println(s"Annotations: ${sym.annotations}")
        //              }
        //            }
        //          }
        //        }).get
        val columns = classDef.symbol.typeSignature.member(nme.CONSTRUCTOR).typeSignature match {
          case MethodType(params, resultType) => params map { param =>
            val cName = param.name.toString()
            val columnName = getNameOfSymbol(param).getOrElse(cName.toUpperCase())
            val columnQName = QualifiedName.columnName(tableQName, columnName)
            val tpe = param.typeSignature
            Column(columnQName, tpe, cName, "case" + cName)
          }
        }
        //        val columns = body.collect({
        //          case valDef @ ValDef(mods, TermName(cNameRaw), tpt: Tree, rhs: Tree) => {
        //            //          case valDef @ DefDef(mods, defName, _, _, tpt: Tree, rhs: Tree) if !defName.equals(nme.CONSTRUCTOR) => {
        //            //            val TermName(cNameRaw) = defName
        //            println(valDef)
        //            println(s"symbol: ${valDef.symbol}")
        //            val cName = cNameRaw.trim // I don't know why, but there's an extra space at the end of value name
        //            val columnName = getNameOfSymbol(valDef.symbol).getOrElse(cName.toUpperCase())
        //            //            val columnName = mods.annotations.headOption.flatMap(annotationToName).getOrElse(cName.toUpperCase())
        //            val columnQName = QualifiedName.columnName(tableQName, columnName)
        //            val tpe = try {
        //              mirror.staticClass("scala." + tpt.toString).toType
        //            } catch {
        //              case e: scala.reflect.internal.MissingRequirementError =>
        //                {
        //                  val columnTypes = Map(
        //                    "Int" -> typeOf[Int], "Double" -> typeOf[Double], "String" -> typeOf[String],
        //                    "scala.Int" -> typeOf[Int], "scala.Double" -> typeOf[Double], "scala.String" -> typeOf[String],
        //                    "java.lang.String" -> typeOf[String], "Boolean" -> typeOf[Boolean], "scala.Boolean" -> typeOf[Boolean])
        //                  columnTypes(tpt.toString)
        //                }
        //            }
        //            Column(columnQName, tpe, cName, "case" + cName)
        //          }
        //        })
        Table(tableQName, columns, Nil, tName + "Table", tName)

      }
    }

    def getYYTableName(table: Table): String = "YY" + table.moduleName

    def createYYTableClass(table: Table): ClassDef = {
      val yyTableName = getYYTableName(table)
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
    def createYYTableModule(table: Table): ModuleDef = {
      val yyTableName = getYYTableName(table)
      val superCall = Apply(Select(Super(This(tpnme.EMPTY), tpnme.EMPTY), nme.CONSTRUCTOR), List(Ident(TermName(table.moduleName))))
      val constructor = DefDef(NoMods, nme.CONSTRUCTOR, List(), List(List()), TypeTree(), Block(List(superCall), Literal(Constant(()))))
      ModuleDef(Modifiers(Flag.IMPLICIT), newTermName("implicit" + yyTableName), Template(List(Ident(TypeName(yyTableName))), emptyValDef, List(constructor)))
    }

    def createLiftedEmbeddingTableClass(table: Table): ClassDef = macroHelper.tableToModule(table) match {
      case ModuleDef(mods, TermName(name), Template(List(_), self, body)) => {
        val tableType = macroHelper.createClassFromString("_root_.scala.slick.driver.JdbcDriver.simple.Table")
        val typeParamName = TypeName(table.caseClassName)
        val tableSuper = AppliedTypeTree(tableType, List(Ident(typeParamName)))
        ClassDef(mods, TypeName(name), Nil, Template(List(tableSuper), self, body))
        //                ModuleDef(mods, TermName(name), Template(List(Ident(TypeName(name))), emptyValDef, Nil)))
      }
    }
    def createLiftedEmbeddingTableModule(table: Table): ModuleDef = {
      val superCall = Apply(Select(Super(This(tpnme.EMPTY), tpnme.EMPTY), nme.CONSTRUCTOR), List())
      val constructor = DefDef(NoMods, nme.CONSTRUCTOR, List(), List(List()), TypeTree(), Block(List(superCall), Literal(Constant(()))))
      //            ModuleDef(Modifiers(Flag.IMPLICIT), TermName(table.moduleName), Template(List(Ident(TypeName(table.moduleName))), emptyValDef, List(constructor)))
      ModuleDef(NoMods, TermName(table.moduleName), Template(List(Ident(TypeName(table.moduleName))), emptyValDef, List(constructor)))
    }

    def createCaseClassRow(table: Table): ClassDef = macroHelper.tableToCaseClass(table) match {
      case ClassDef(mods, name, tparams, Template(parents, self, body)) => {
        //              val trTree = macroHelper.typeToTree(typeOf[YYTableRow])
        val trTree = macroHelper.createClassFromString("_root_.scala.slick.yy.YYTableRow")
        ClassDef(mods, name, tparams, Template(trTree :: parents, self, body))
      }
    }

    def createRepToTableImplicitDef(table: Table): DefDef = {
      val yyTableName = getYYTableName(table)
      val repTypeName = newTypeName("CakeRep")
      val tableTypeName = newTypeName(yyTableName)
      //            val body = TypeApply(Select(Ident(newTermName("x")), newTermName("asInstanceOf")), List(Ident(tableTypeName)))
      val body = Apply(Select(New(Ident(tableTypeName)), nme.CONSTRUCTOR), List(TypeApply(Select(Select(Ident(newTermName("x")), newTermName("underlying")), newTermName("asInstanceOf")), List(Ident(newTypeName(table.moduleName))))))
      DefDef(Modifiers(Flag.IMPLICIT), newTermName("convImplicit" + yyTableName), List(), List(List(ValDef(Modifiers(PARAM), newTermName("x"), AppliedTypeTree(Ident(repTypeName), List(Ident(newTypeName(table.caseClassName)))), EmptyTree))), Ident(tableTypeName), body)
    }

    def convertCaseClass(tree: Tree) = {

      tree match {
        case classDef @ ClassDef(mods, _, _, _) if isCaseClassDef(tree) => {
          val table = getTableFromCaseClassDef(classDef)
          //                    println(table)
          val caseClassDef = createCaseClassRow(table)
          val tableClassDef = createLiftedEmbeddingTableClass(table)
          val tableModuleDef = createLiftedEmbeddingTableModule(table)
          val yyTableClassDef = createYYTableClass(table)
          val yyTableImplicitModule = createYYTableModule(table)
          val yyRepToTableImplicit = createRepToTableImplicitDef(table)
          //          List(caseClassDef)
          //          List(caseClassDef, tableClassDef)
          //          List(caseClassDef, tableClassDef, yyTableClassDef)
          //          List(caseClassDef, tableClassDef, tableModuleDef, yyTableClassDef)
          //          List(caseClassDef, tableClassDef, tableModuleDef, yyTableClassDef, yyTableImplicitModule)
          List(caseClassDef, tableClassDef, tableModuleDef, yyTableClassDef, yyTableImplicitModule, yyRepToTableImplicit)
          //          List(caseClassDef, tableClassDef, tableModuleDef, yyTableClassDef, yyTableImplicitVal)
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
//import scala.annotation.ClassfileAnnotation
//final case class Entity2(name: String) extends ClassfileAnnotation