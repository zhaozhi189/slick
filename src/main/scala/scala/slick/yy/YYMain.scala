package scala.slick.yy

object YYMain {

  def main(args: Array[String]) {
    YYTest()
  }
}

object YYTest extends YYSlickCake {
  import scala.slick.driver.H2Driver.simple._
  import Database.threadLocalSession
  import TestTable.TableA
  import TestTable.YYTableA
  import TestTable.underlying

  def apply() {

    Database.forURL("jdbc:h2:mem:test1", driver = "org.h2.Driver") withSession {
      (TableA.ddl).create

      TableA.insert((14, 1))
      TableA.insert((18, 1))
      TableA.insert((15, 2))
      TableA.insert((20, 3))

      val yt = YYTableA
      val yq = YYQuery(yt)
      val y15 = YYConstColumn(15)
      val y16 = YYConstColumn(16)

      println(yq.query.list)

      val yr = yq.map(x => x)

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

      val yrLength = YYQuery(yq.length)

      println(yrLength.query.first)
    }
  }

}