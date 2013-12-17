package scala.slick.model.codegen

import scala.slick.{model => m}
import scala.slick.model.ForeignKeyAction
import scala.slick.ast.ColumnOption
import scala.slick.SlickException
import scala.reflect.runtime.universe._
import scala.annotation.StaticAnnotation
/** Annotation for storing scaladoc comments in the tree */
//final case class doc(doc:String = "") extends StaticAnnotation

/** Base implementation for a Source code tree generator */
abstract class AbstractTreeGenerator(model: m.Model)
                   extends AbstractGenerator[Tree,TermName,TypeName](model)
                   with TreeGeneratorHelpers{
  codegen =>
  /** Generates code for the complete model (not wrapped in a package yet)
      @group Basic customization overrides */
  def codeTrees : Seq[Tree] = Seq(
    q"import scala.slick.model.ForeignKeyAction"
    //q"import scala.slick.model.codegen.doc"
  ) ++
    ( if(tables.exists(_.hlistEnabled)){ Seq(
        q"import scala.slick.collection.heterogenous._",
        q"import scala.slick.collection.heterogenous.syntax._"
      )} else Seq()
    ) ++
    ( if(tables.exists(_.PlainSqlMapper.enabled)){ Seq(
        q"import scala.slick.jdbc.{GetResult => GR}",
        {val s = "NOTE: GetResult mappers for plain SQL are only generated for tables where Slick knows how to map the types of all columns."
         q"$s"}
      )} else Seq()
    ) ++
    tables.flatMap(_.code)

  private def tuple(i: Int) = q"""${newTermName(s"_${i+1}")}"""

  abstract class TableDef(model: m.Table) extends super.TableDef(model){
    import scala.language.implicitConversions
    implicit def names2trees(names: Seq[Name]): Seq[Tree] = names.map(n=>q"$n")

    def compound(valuesOrTypes: Seq[Tree]): Tree = {
      if(hlistEnabled) valuesOrTypes.reverse.foldLeft(q"HNil":Tree)((v,e) => q"$e :: $v" )
      else if (valuesOrTypes.size == 1) valuesOrTypes.head
      else if(valuesOrTypes.size <= 22) q"""(..${valuesOrTypes})"""
      else throw new Exception("Cannot generate tuple for > 22 columns, please set hlistEnable=true or override compound.")
    }

    def factory   = if(columns.size == 1) q"${EntityType.termName}" else q"${EntityType.termName}.tupled"
    def extractor = q"${EntityType.termName}.unapply"

    def EntityType: EntityType
    type EntityType <: EntityTypeDef
    trait EntityTypeDef extends super.EntityTypeDef{
      def termName = codegen.termName(rawName)
      def code = 
        if(classEnabled){
          val args = columns.map(c=>
            c.default.map( v =>
              q"val ${c.name}: ${c.exposedType}=$v"
            ).getOrElse(
              q"val ${c.name}: ${c.exposedType}"
            )
          )
          q"""case class $name(..$args) extends ..$parents"""
        } else {
          q"type $name = $types"
        }
    }

    trait PlainSqlMapperDef extends super.PlainSqlMapperDef{
      def code = {
        val positional = compound(columnsPositional.map(c => (if(c.fakeNullable || c.model.nullable)q"<<?[${c.rawType}]"else q"<<[${c.rawType}]")))
        val dependencies = columns.map(_.rawType).toSet.toList.zipWithIndex.map{ case (t,i) => q"""implicit val ${newTermName("e"+i)}: GR[$t]"""}
        val rearranged = compound(desiredColumnOrder.map(i => if(hlistEnabled) q"r($i)" else tuple(i)))
        def result(args: Tree) = if(mappingEnabled) q"$factory($args)" else args
        val body =
          if(autoIncLastAsOption && columns.size > 1){
            q"""
val positional = $positional
import positional._
${result(rearranged)} // putting AutoInc last
            """
          } else
           result(positional)
        q"""
implicit def ${name}(..$dependencies): GR[${TableClass.elementType}] = GR{
  prs => import prs._
  ${body}
}
        """
      }
    }

    trait TableClassDef extends super.TableClassDef{
      def star = {
        val struct = compound(columns.map(c=>if(c.fakeNullable)q"${c.name}.?" else q"${c.name}"))
        val rhs = if(mappingEnabled) q"$struct <> ($factory, $extractor)" else struct
        q"def * = $rhs"
      }
      def option = {
        val struct = compound(columns.map(c=>if(c.model.nullable)q"${c.name}" else q"${c.name}.?"))
        val rhs = if(mappingEnabled) q"""$struct.shaped.<>($optionFactory, (_:Any) =>  throw new Exception("Inserting into ? projection not supported."))""" else struct
        q"def ? = $rhs"
      }
      def optionFactory = {
        val positionalAccessors = columns.zipWithIndex.map{ case(c,i) =>
          if(c.fakeNullable || c.model.nullable) tuple(i) else q"${tuple(i)}.get"
        }
        val fac = q"$factory(${compound(positionalAccessors)})"
        val discriminator = columns.zipWithIndex.collect{ case (c,i) if !c.model.nullable => tuple(i) }.headOption
        val expr = discriminator.map(d => q"$d.map(_=> $fac)").getOrElse(q"None")
        q"{r=>import r._; $expr}"
      }
      def code = {
        val args = model.name.schema.map(n => q"Some($n)") ++ Seq(q"${model.name.table}")
        q"""
class $name(tag: Tag) extends Table[$elementType](tag, ..$args) with ..$parents{
  ..${body.flatten:Seq[Tree]}
}
        """
      }
    }

    trait TableValueDef extends super.TableValueDef{
      def code = q"lazy val $name = new TableQuery(new ${TableClass.name}(_))" // FIXME: why doesn't the macro work here?
    }

    class ColumnDef(model: m.Column) extends super.ColumnDef(model){
      import ColumnOption._
      def columnOptionCode = {
        case PrimaryKey     => Some(q"O.PrimaryKey")
        case Default(value) => Some(q"O.Default(${default.get})") // .get is safe here
        case DBType(dbType) => Some(q"O.DBType($dbType)")
        case AutoInc        => Some(q"O.AutoInc")
        case NotNull|Nullable => throw new SlickException( s"Please don't use Nullable or NotNull column options. Use an Option type, respectively the nullable flag in Slick's model model Column." )
        case o => throw new SlickException( s"Don't know how to generate code for unexpected ColumnOption $o." )
      }
      def defaultCode = {
        case Some(v) => q"Some(${defaultCode(v)})"
        case s:String  => q"$s"
        case None      => q"None"
        case v:Int     => q"$v"
        case v:Long    => q"$v"
        case v:Float   => q"$v"
        case v:Double  => q"$v"
        case v:Boolean => q"$v"
        case v:Short   => q"$v"
        case v => throw new SlickException( s"Dont' know how to generate code for default value $v of ${v.getClass}" )
      }
      // Explicit type to allow overloading existing Slick method names.
      // Explicit type argument for better error message when implicit type mapper not found.
      def code = q"""val $name: Column[$actualType] = column[$actualType](${model.name},..$options)"""
    }

    class PrimaryKeyDef(model: m.PrimaryKey) extends super.PrimaryKeyDef(model){
      def code = q"""val $name = primaryKey($dbName, ${compound(columns.map(_.name))})"""
    }

    class ForeignKeyDef(model: m.ForeignKey) extends super.ForeignKeyDef(model){
      def actionCode(action: ForeignKeyAction) = action match{
        case ForeignKeyAction.Cascade    => q"ForeignKeyAction.Cascade"
        case ForeignKeyAction.Restrict   => q"ForeignKeyAction.Restrict"
        case ForeignKeyAction.NoAction   => q"ForeignKeyAction.NoAction"
        case ForeignKeyAction.SetNull    => q"ForeignKeyAction.SetNull"
        case ForeignKeyAction.SetDefault => q"ForeignKeyAction.SetDefault"
      }
      def code = {
        val fkColumns = compound(referencingColumns.map(_.name))
        val pkTable = referencedTable.TableValue.name
        val pkColumns = compound(referencedColumns.map(c => q"r.${c.name}"))
        q"""val $name = foreignKey($dbName, $fkColumns, $pkTable)(r => $pkColumns, onUpdate=${onUpdate}, onDelete=${onDelete})"""
      }
    }

    class IndexDef(model: m.Index) extends super.IndexDef(model){
      def code = {
        val unique = if(model.unique) Seq(q"unique=true") else Seq()
        q"""val $name = index($dbName, ${compound(columns.map(_.name))}, ..$unique)"""
      }
    }
  }
}

trait TreeGeneratorHelpers extends scala.slick.model.codegen.GeneratorHelpers[Tree,TermName,TypeName]{
  def docWithCode(doc: String, code:Tree): Tree = (if(doc != "") q"$code" else code) // TODO: get docs in annotations
  final def optionType(t: Tree): Tree = tq"""Option[$t]"""
  def termName( name: String ) = newTermName(name)
  def typeName( name: String ) = newTypeName(name)
  /** Creates a scala tree from a String decribing a qualified term */
  def parseTerm(term: String): Tree = {
    val path = term.split("\\.")
    if(path.length > 1){
      val location = 
        path.tail.dropRight(1).foldLeft( q"${newTermName(path.head)}": Tree )(
          (from, selectee) => q"${from}.${newTermName(selectee)}"
        )
      q"${location}.${newTermName(path.last)}"
    } else {
      q"${newTermName(path.last)}"
    }    
  }
  /** Creates a scala tree from a String decribing a qualified type */
  def parseType(tpe: String): Tree = {
/*    val TypeMatcher = "^([\\[]*)(\\[(.*)\\])?$".r
    val (baseType,typeArgs) = tpe.trim match {
      case TypeMatcher(baseType,_,typeArgs) = (baseType,typeArgs)
      case _ => "Cannot parse type: "+tpe
    }*/
    val path = tpe.split("(\\.|\\#)")
    if(path.length > 1){
      val location = 
        path.tail.dropRight(1).foldLeft( q"${newTermName(path.head)}": Tree )(
          (from, selectee) => q"${from}.${newTermName(selectee)}"
        )
      q"${location}.${newTypeName(path.last)}"
    } else {
      q"${newTypeName(path.last)}"
    }    
  } 
}