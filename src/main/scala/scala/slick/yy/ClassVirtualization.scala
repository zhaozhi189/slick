package scala.slick.yy

import scala.slick.schema.Table
import scala.slick.schema.Column
import scala.slick.schema.QualifiedName
import scala.slick.typeproviders.MacroHelpers
import scala.slick.typeproviders.DefaultContextUtils
import scala.reflect.api.Universe
import scala.reflect.api.Mirror
import scala.collection.mutable.ArrayBuffer

trait YYTransformers {
  val universe: Universe
  val mirror: universe.Mirror

  import universe._
  import Flag.CASE
  import Flag.PARAM

  val macroHelper = new {
    val universe: YYTransformers.this.universe.type = YYTransformers.this.universe
  } with MacroHelpers(DefaultContextUtils, "")

  var virtualTraverserIsApplied: Boolean = false
  var virtualSymbols: List[Symbol] = Nil

  object ClassVirtualization extends (Tree => Tree) {
    def apply(tree: Tree): Tree = {
      val cv = new ClassVirtualization()
      if (!virtualTraverserIsApplied)
        cv.transform(tree)
      else {
        cv.transformBySymbols(tree, virtualSymbols)
      }
    }
  }
  // workaround for compatibility of 2.10 and macro paradise
  object TermName {
    def apply(s: String): TermName = newTermName(s)
    def unapply(t: TermName): Option[String] = Some(t.toString)
  }
  object TypeName {
    def apply(s: String): TypeName = newTypeName(s)
    def unapply(t: TypeName): Option[String] = Some(t.toString)
  }

  class ClassVirtualization extends Transformer {

    def isCaseClassDef(tree: Tree): Boolean = tree match {
      case ClassDef(mods, _, _, _) if mods.hasFlag(CASE) => true
      case _ => false
    }

    val SYNTHETIC = scala.reflect.internal.Flags.SYNTHETIC.asInstanceOf[Long].asInstanceOf[FlagSet]

    def isCaseClassObject(tree: Tree): Boolean = tree match {
      case ModuleDef(mods, _, _) if mods.hasFlag(SYNTHETIC) => true
      case _ => false
    }

    def getTableFromSymbol(symbol: Symbol) = {
      def getNameOfSymbol(symbol: Symbol): Option[String] = {
        // workaround for SI-7424
        symbol.typeSignature
        symbol.annotations.foreach(_.tpe)

        symbol.annotations.headOption.map(_.scalaArgs.head).flatMap(annotationToName)
      }

      def annotationToName(tree: Tree): Option[String] =
        tree match {
          case Literal(Constant(name: String)) => Some(name)
          case _ => None
        }
      val tName = symbol.name.toString()
      val tableName = getNameOfSymbol(symbol).getOrElse(tName.toUpperCase())
      val tableQName = QualifiedName.tableName(tableName)
      val columns = symbol.typeSignature.member(nme.CONSTRUCTOR).typeSignature match {
        case MethodType(params, resultType) => params map { param =>
          val cName = param.name.toString()
          val columnName = getNameOfSymbol(param).getOrElse(cName.toUpperCase())
          val columnQName = QualifiedName.columnName(tableQName, columnName)
          val tpe = param.typeSignature
          Column(columnQName, tpe, cName, "case" + cName)
        }
      }
      Table(tableQName, columns, Nil, tName + "Table", tName)
    }

    def getTableFromCaseClassDef(classDef: ClassDef) = getTableFromSymbol(classDef.symbol)

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
      }
    }
    def createLiftedEmbeddingTableModule(table: Table): ModuleDef = {
      val superCall = Apply(Select(Super(This(tpnme.EMPTY), tpnme.EMPTY), nme.CONSTRUCTOR), List())
      val constructor = DefDef(NoMods, nme.CONSTRUCTOR, List(), List(List()), TypeTree(), Block(List(superCall), Literal(Constant(()))))
      ModuleDef(NoMods, TermName(table.moduleName), Template(List(Ident(TypeName(table.moduleName))), emptyValDef, List(constructor)))
    }

    def createCaseClassRow(table: Table): ClassDef = macroHelper.tableToCaseClass(table) match {
      case ClassDef(mods, name, tparams, Template(parents, self, body)) => {
        val trTree = macroHelper.createClassFromString("_root_.scala.slick.yy.YYTableRow")
        ClassDef(mods, name, tparams, Template(trTree :: parents, self, body))
      }
    }

    def createRepToTableImplicitDef(table: Table): DefDef = {
      val yyTableName = getYYTableName(table)
      val repTypeName = newTypeName("CakeRep")
      val tableTypeName = newTypeName(yyTableName)
      val body = Apply(Select(New(Ident(tableTypeName)), nme.CONSTRUCTOR), List(TypeApply(Select(Select(Ident(newTermName("x")), newTermName("underlying")), newTermName("asInstanceOf")), List(Ident(newTypeName(table.moduleName))))))
      DefDef(Modifiers(Flag.IMPLICIT), newTermName("convImplicit" + yyTableName), List(), List(List(ValDef(Modifiers(PARAM), newTermName("x"), AppliedTypeTree(Ident(repTypeName), List(Ident(newTypeName(table.caseClassName)))), EmptyTree))), Ident(tableTypeName), body)
    }

    def getTreesFromTable(table: Table): List[Tree] = {
      val caseClassDef = createCaseClassRow(table)
      val tableClassDef = createLiftedEmbeddingTableClass(table)
      val tableModuleDef = createLiftedEmbeddingTableModule(table)
      val yyTableClassDef = createYYTableClass(table)
      val yyTableImplicitModule = createYYTableModule(table)
      val yyRepToTableImplicit = createRepToTableImplicitDef(table)
      List(caseClassDef, tableClassDef, tableModuleDef, yyTableClassDef, yyTableImplicitModule, yyRepToTableImplicit)
    }

    def convertCaseClass(tree: Tree) = {

      tree match {
        case classDef @ ClassDef(mods, _, _, _) if isCaseClassDef(tree) => {
          val table = getTableFromCaseClassDef(classDef)
          getTreesFromTable(table)
        }
        case _ => Nil
      }
    }

    override def transform(tree: Tree): Tree = tree match {
      case Block(stats, expr) if (stats.exists(isCaseClassDef(_))) => {
        Block(stats.flatMap(x => if (isCaseClassDef(x)) convertCaseClass(x) else if (isCaseClassObject(x)) Nil else List(transform(x))), transform(expr))
      }
      case _ => super.transform(tree)
    }
    def transformBySymbols(tree: Tree, symbols: List[Symbol]): Tree = tree match {
      case Block(stats, expr) => {
        Block(symbols.flatMap((getTableFromSymbol _) andThen getTreesFromTable) ++ stats, expr)
      }
      case expr => Block(symbols.flatMap((getTableFromSymbol _) andThen getTreesFromTable), expr)
    }
  }

  object VirtualClassCollector {
    def apply(tree: Tree): List[Symbol] = {
      val vcc = new VirtualClassCollector()
      vcc.traverse(tree)
      virtualTraverserIsApplied = true
      virtualSymbols = vcc.collected.toList
      virtualSymbols
    }
  }

  private final class VirtualClassCollector extends Traverser {

    private[YYTransformers] val collected = new ArrayBuffer[Symbol]()

    override def traverse(tree: Tree) = tree match {
      case TypeApply(Select(shallowTable, TermName("getTable")), List(tpt)) if shallowTable.symbol.typeSignature =:= typeOf[Shallow.Table.type] => {
        //      case TypeApply(Select(shallowTable, TermName("getTable")), List(tpt)) => {
        //        println(shallowTable.symbol.typeSignature)
        //        println(typeOf[Shallow.Table.type].typeSymbol)
        //        println(shallowTable.symbol.equals(typeOf[Shallow.Table.type].typeSymbol))
        //        println(new ClassVirtualization().getTableFromSymbol(tpt.symbol))
        collected += tpt.symbol
      }
      case TypeApply(Select(shallowQueryable, TermName("apply")), List(tpt)) if shallowQueryable.symbol.typeSignature =:= typeOf[Shallow.Queryable.type] => {
        collected += tpt.symbol
      }
      case _ => super.traverse(tree)
    }

  }
}

import scala.annotation.StaticAnnotation
final case class Entity(name: String) extends StaticAnnotation
