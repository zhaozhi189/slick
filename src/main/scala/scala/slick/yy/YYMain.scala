package scala.slick.yy

import scala.language.implicitConversions

object YYMain {

  def main(args: Array[String]) {
    YYTest()
    Seq(1).map(x => x)
  }
}

object YYTest extends YYSlickCake {
  import scala.slick.driver.H2Driver
  import scala.slick.driver.H2Driver.simple._
  import H2Driver.simple.Database.threadLocalSession
  import TestTable.TableA
  import TestTable.YYTableA
  import TestTable.underlying
  import TestTable.convertTuple2ToTableARow

  def apply() {
    implicit val driver = H2Driver

    Database.forURL("jdbc:h2:mem:test1", driver = "org.h2.Driver") withSession {
      (TableA.ddl).create

      TableA.insert((14, 1))
      TableA.insert((18, 1))
      TableA.insert((15, 2))
      TableA.insert((20, 3))
      
      TableA.map( x => x.id + 1)

      val yt = YYTableA
      val yq = YYQuery(yt)
      val y15 = YYConstColumn(15)
      val y16 = YYConstColumn(16)
/*
      println(yq.toSeq)
//      println(yq.toSeq(H2Driver))

      val yr = yq.map(x => x)

      println(yr.toSeq)
//      println(yr.toSeq(H2Driver))

      val yrMap = yq.map(x => YYColumn(underlying(x).id))

      println(yrMap.toSeq)
//      println(yrMap.toSeq(H2Driver))

      val yrTuple = yq.map(x => YYProjection(underlying(x).id, underlying(x).grade))

      println(yrTuple.toSeq)
//      println(yrTuple.toSeq(H2Driver))

      val yrFilter1 = yq filter (x => YYColumn(underlying(x).id) === y15) map
        (x => YYColumn(underlying(x).grade))

        println(yrFilter1.toSeq)
//      println(yrFilter1.toSeq(H2Driver))

      val yrFilter2 = yq filter (x => YYColumn(underlying(x).id) > y16) map
        (x => YYColumn(underlying(x).grade))

        println(yrFilter2.toSeq)
//      println(yrFilter2.toSeq(H2Driver))

      val yrLength = YYQuery(yq.length)

      println(yrLength.first)
//      println(yrLength.first(H2Driver))

      val yrSingleColumn = YYQuery(y15)

      println(yrSingleColumn.toSeq)
//      println(yrSingleColumn.toSeq(H2Driver))

      val yrSCFilter1 = yrSingleColumn.filter(x => y16 > x.asInstanceOf[YYColumn[Int]])
      val yrSCFilter2 = yrSingleColumn.filter(x => y15 > x.asInstanceOf[YYColumn[Int]])

      println(yrSCFilter1.toSeq)
      println(yrSCFilter2.toSeq)
//      println(yrSCFilter1.toSeq(H2Driver))
//      println(yrSCFilter2.toSeq(H2Driver))
 */
    }
  }

}