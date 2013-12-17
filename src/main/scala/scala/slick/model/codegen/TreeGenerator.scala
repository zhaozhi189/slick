package scala.slick.model.codegen

import scala.slick.{model => m}

import scalariform.formatter._
import scala.reflect.runtime.universe._

/**
 * A code generator generating Scala trees
 * using sprinter and scalariform for pretty printing.
 */
class TreeGenerator(model: m.Model)
                   extends AbstractTreeGenerator(model) with OutputHelpers{
  // "Tying the knot": making virtual classes concrete
  type Table = TableDef
  def Table = new TableDef(_)
  class TableDef(model: m.Table) extends super.TableDef(model){
    // Using defs instead of (caching) lazy vals here to provide consitent interface to the user.
    // Performance should really not be critical in the code generator. Models shouldn't be huge.
    // Also lazy vals don't inherit docs from defs
    type EntityType     =     EntityTypeDef
    def  EntityType     = new EntityType{}
    type PlainSqlMapper =     PlainSqlMapperDef
    def  PlainSqlMapper = new PlainSqlMapper{}
    type TableClass     =     TableClassDef
    def  TableClass     = new TableClass{}
    type TableValue     =     TableValueDef
    def  TableValue     = new TableValue{}
    type Column         =     ColumnDef
    def  Column         = new Column(_)
    type PrimaryKey     =     PrimaryKeyDef
    def  PrimaryKey     = new PrimaryKey(_)
    type ForeignKey     =     ForeignKeyDef  
    def  ForeignKey     = new ForeignKey(_)
    type Index          =     IndexDef  
    def  Index          = new Index(_)
  }

  def code = codeTrees.map(Unparser.unparse).mkString("\n\n")
  override def packageCode(driver: String, pkg: String, obj: String) = {
    try{
      ScalaFormatter.format(super.packageCode(driver, pkg, obj))
    } catch {
      case e:Exception => throw e
    }
  }
}
