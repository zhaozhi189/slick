package scala.slick.yy

import scala.slick.ast.Dump
import scala.slick.ast.StaticType
import scala.slick.driver.H2Driver
import scala.slick.ast.TypedType
import scala.slick.ast.CollectionType
import scala.slick.ast.CollectionTypeConstructor
import scala.slick.ast.TableNode
import scala.slick.ast.Node
import scala.slick.ast.WithOp
import scala.slick.ast.TypedNode
import scala.slick.ast.Type
import scala.slick.ast.SymbolScope
import scala.slick.ast.Select
import scala.slick.ast.FieldSymbol
import scala.slick.ast.Ref

object YYMain extends NumericOps with TupleOps {

  def main0(args: Array[String]): Unit = {
    val a = NumInt(2)
    val b = NumInt(5)
    val c = a + b
    Dump(c)
    val tuple1 = new YYTuple2(a, b)
    Dump(tuple1)
    Dump(tuple1._1)
    val tuple2 = new YYTuple2(c, a)
  }

  def main(args: Array[String]): Unit = {
    //    val intTpe = H2Driver.columnTypes.intJdbcType
    import H2Driver.Implicit._
    val intTpe = implicitly[TypedType[Int]]
    val collTpe = CollectionType(CollectionTypeConstructor.default, intTpe)
    val tableNode = createTable("TABLE_A", collTpe, "A_ID", intTpe)

    val compiled = H2Driver.selectStatementCompiler.run(tableNode).tree
    Dump(compiled)
    testGeneratedNode(compiled)
  }

  def testGeneratedNode(node: Node) {
    import scala.slick.driver.H2Driver.simple._
    import Database.threadLocalSession

    object TableA extends Table[(Int)]("TABLE_A") {
      def id = column[Int]("A_ID")
      def * = id
    }

    Database.forURL("jdbc:h2:mem:test1", driver = "org.h2.Driver") withSession {
      (TableA.ddl).create

      TableA.insert(2)
      println(H2Driver.createQueryInvoker(node).first)
    }
  }

  def createTable(name: String, _tpe: Type, columnName: String, columnTpe: Type): TableNode = new TableNode with WithOp with TypedNode { table =>
    def tpe = _tpe
    def nodeTableProjection: Node =
      createColumn(table, columnName, columnTpe)
    def schemaName: Option[String] = None
    def tableName: String = name
    override def nodeWithComputedType(scope: SymbolScope, retype: Boolean) =
      super[TypedNode].nodeWithComputedType(scope, retype)
  }

  def createColumn(tableNode: TableNode, columnName: String, columnTpe: Type): Node = {
    val in = Node(tableNode) match {
      case r: Ref => r
      case _ => Ref(Node(tableNode).nodeIntrinsicSymbol)
    }
    Select(in, FieldSymbol(columnName)(Seq(), columnTpe)).nodeTyped(columnTpe)
  }

}