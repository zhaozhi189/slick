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

object YYMain {

  def main(args: Array[String]) {
    import scala.slick.driver.H2Driver.simple._
    import Database.threadLocalSession
    import scala.slick.lifted.Projection

    object TableA extends Table[(Int, Int)]("TABLE_A") {
      def id = column[Int]("A_ID")
      def grade = column[Int]("A_GRADE")
      def * = id ~ grade
    }

    Database.forURL("jdbc:h2:mem:test1", driver = "org.h2.Driver") withSession {
      (TableA.ddl).create

      TableA.insert((14, 1))
      TableA.insert((16, 1))
      TableA.insert((18, 2))
      TableA.insert((20, 3))

      val yt = YYTable(TableA)
      //      val yq = YYQuery(TableA) // deprecated! :)
      val yq = YYQuery(yt)
      val yId = YYColumn(TableA.id)
      val y15 = YYConstColumn(15)

      println(yq.query.list)

      val yr = yq.map(x => x)

      println(yr.query.list)

      val yrMap = yq.map(x => YYValue(x.underlying.asInstanceOf[TableA.type].id))

      println(yrMap.query.list)

      val yrTuple = yq.map(x => YYProjection(x.underlying.asInstanceOf[TableA.type].id, x.underlying.asInstanceOf[TableA.type].grade))

      println(yrTuple.query.list)
    }
  }
}