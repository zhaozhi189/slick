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

object YYMain extends YYSlickCake {

  def main(args: Array[String]) {
    import scala.slick.driver.H2Driver.simple._
    import Database.threadLocalSession
    import TestTable.TableA
    import TestTable.YYTableA
    import TestTable.underlying

    Database.forURL("jdbc:h2:mem:test1", driver = "org.h2.Driver") withSession {
      (TableA.ddl).create

      TableA.insert((14, 1))
      TableA.insert((15, 1))
      TableA.insert((18, 2))
      TableA.insert((20, 3))

      val yt = YYTableA
      val yq = YYQuery(yt)
      val y15 = YYConstColumn(15)
      val y16 = YYConstColumn(16)

      println(yq.query.list)

      val yr = yq.map(x => x)

      println(yr.query.list)

      val yrMap = yq.map(x => YYColumn(underlying(x).id))

      println(yrMap.query.list)

      val yrTuple = yq.map(x => YYProjection(underlying(x).id, underlying(x).grade))

      println(yrTuple.query.list)

      val yrFilter1 = yq filter (x => YYColumn(underlying(x).id) === y15) map
        (x => YYColumn(underlying(x).grade))

      println(yrFilter1.query.list)

      val yrFilter2 = yq filter (x => YYColumn(underlying(x).id) > y16) map
        (x => YYColumn(underlying(x).grade))

      println(yrFilter2.query.list)
    }
  }
}