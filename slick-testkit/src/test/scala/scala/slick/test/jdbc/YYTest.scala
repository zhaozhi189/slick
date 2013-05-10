package scala.slick.test.jdbc

import scala.language.implicitConversions
import org.junit.Test
import org.junit.Assert._
import scala.slick.yy._

class YYTest {

  val jdbcTypes = new scala.slick.driver.JdbcDriver.ImplicitJdbcTypes()

  @Test def simpleTest() {
    import Shallow._
    val y = 5.3
    val r1 = slickYY {
      val q = Query(y)
      q.toSeq
    }
    assertEquals("Query of int", y, r1.head, 0.1)
    val r2 = slickYY {
      Query(y).map(x => x).toSeq
    }
    assertEquals("Query identity map", y, r2.head, 0.1)
    val r3 = slickYY {
      Query(y).map(x => false).toSeq
    }
    assertEquals("Query dummy map", false, r3.head)
    val r4 = slickYY {
      Query(y).filter(x => x < 1).toSeq
    }
    assertEquals("Query filter", 0, r4.length)

    val z = 3
    val r5 = slickYY {
      Query(z).filter(x => x > 2.5).toSeq
    }
    assertEquals("Query filter + captured var", z, r5.head)
    val a = 1
    val r6 = slickYY {
      val b = 1
      Query(a).filter(x => x == b).toSeq
    }
    assertEquals("Query filter + Column ==", a, r6.head)
    val r7 = slickYY {
      val b = 1
      Query(2 > b).filter(x => x == true).toSeq
    }
    assertEquals("Query filter + Column == (2)", true, r7.head)
  }

  @Test
  def tuple2Test() {
    import Shallow._
    val r1 = slickYY {
      val x = (1, 2)
      val q = Query(x)
      q.first
    }
    assertEquals("Query of tuple2", (1, 2), r1)
    val r2 = slickYY {
      val x = (1, 2.5)
      Query(x).map(x => x._2).first
    }
    assertEquals("Query map _2 of tuple", 2.5, r2, 0.1)
    val r3 = slickYY {
      val x = (1, 2)
      val q = Query(x).map(x => x._2)
      val q2 = q.filter(x => x == 1)
      q2.toSeq
    }
    assertEquals("Query filter of tuple2 + Column ==", 0, r3.length)
    val r4 = slickYY {
      val x = (1, 2)
      Query(x).map(x => x._2).filter(x => x == 2).toSeq
    }
    assertEquals("Query filter of tuple2 + Column == (2)", 1, r4.length)
    val r5 = slickYY {
      val x = (1, 2)
      Query(x).map(x => x._2).filter(x => x > 1).toSeq
    }
    assertEquals("Query filter of tuple2 + Column >", 1, r5.length)
    val r6 = slickYY {
      Query((1, 2)).map(x => (x._2, if (x._2 == 2) false else true)).toSeq
    }
    assertEquals("Query map of tuple 2 + Column > + if true", (2, false), r6.head)
    val r7 = slickYY {
      Query((1, 2)).map(x => (x._2, if (x._2 == 1) false else true)).toSeq
    }
    assertEquals("Query map of tuple 2 + Column > + if false", (2, true), r7.head)
  }

  @Test def testTableTest() {
    import Shallow._
    initTable()
    val r1 = slickYY {
      val tbl = Table.test()
      val q = Query.ofTable(tbl)
      q.toSeq
    }
    assertEquals("Query of Table", 4, r1.length)
    val r2 = slickYY {
      val tbl = Table.test()
      val q = Query.ofTable(tbl).map(x => x)
      q.toSeq
    }
    assertEquals("Query identity map of Table", 4, r2.length)

    val r3 = slickYY {
      val tbl = Table.test2()
      val q = Query.ofTable(tbl) map (x => x.id)
      q.toSeq
    }
    assertEquals("Query map _1 of Table", List(14, 18, 15, 20), r3.toList)

    val r4 = slickYY {
      val tbl = Table.test2()
      val q = Query.ofTable(tbl) map (x => x.id) filter (x => x < 16) filter
        (x => x > 14)
      q.toSeq
    }
    assertEquals("Query filter map _1 of Table", List(15), r4.toList)

    val r5 = slickYY {
      val tbl = Table.test2()
      val q = Query ofTable tbl filter (x => x.id == 14)
      q.toSeq
    }
    assertEquals("Query filter of Table", List(YYTableARow(14, 1)), r5.toList)

    val r6 = slickYY {
      val tbl = Table.test2()
      val q = Query ofTable tbl map (x => (x.id, x.grade))
      q.toSeq
    }
    assertEquals("Query map (_1, _2) of Table", List((14, 1), (18, 1), (15, 2), (20, 3)), r6.toList)

    val r7 = slickYY {
      val tbl = Table.test2()
      val q = Query ofTable tbl map (x => (x.id, if (x.grade == 1) "One" else "More"))
      q.toSeq
    }
    assertEquals("Query map (_1, _2) of Table + if", List((14, "One"), (18, "One"), (15, "More"), (20, "More")), r7.toList)

    val r8 = slickYY {
      val tbl = Table.getTable[TableARow]
      val q = Query.ofTable(tbl) map (x => x.id)
      q.toSeq
    }
    assertEquals("Query map _1 of Table by getTable", List(14, 18, 15, 20), r8.toList)

    YYUtils.closeSession
  }

  @Test
  def virtualizationTest {
    initCoffeeTable()
    import jdbcTypes._
    import Shallow._
    val r1 = slickYYV {
      case class Coffee(id: Int, name: String);
      val tbl = Table.getTable[Coffee]
      val q = Query.ofTable(tbl)
      val q1 = q map (x => x.id)
      q1.toSeq
    }
    assertEquals("Query map _1 of Virtualized Table", 4, r1.length)
    val r2 = slickYYV {
      case class Coffee(id: Int, name: String);
      val tbl = Table.getTable[Coffee]
      val q = Query.ofTable(tbl)
      val q1 = q map (x => (x.id, x.name))
      q1.toSeq
    }
    assertEquals("Query map (_1, _2) of Table", List((1, "one"), (2, "two"), (3, "three"), (10, "ten")), r2.toList)
    val r3 = slickYYV {
      case class Coffee(id: Int, name: String);
      val tbl = Table.getTable[Coffee]
      val q = Query.ofTable(tbl)
      val q1 = q map (x => (x.id, if (x.id < 3) "Low" else x.name))
      q1.toSeq
    }
    assertEquals("Query map (_1, _2) of Table + if", List((1, "Low"), (2, "Low"), (3, "three"), (10, "ten")), r3.toList)
    val r4 = slickYYV {
      @Entity("COFFEE") case class Coff(@Entity("ID") idNumber: Int, name: String);
      val tbl = Table.getTable[Coff]
      val q = Query.ofTable(tbl)
      val q1 = q map (x => (x.idNumber, x.name))
      q1.toSeq
    }
    assertEquals("Query map (_1, _2) of Table + Annotation", List((1, "one"), (2, "two"), (3, "three"), (10, "ten")), r4.toList)
    val r5 = slickYYV {
      @Entity("COFFEE") case class Coff(@Entity("ID") idNumber: Int, name: String);
      val tbl = Table.getTable[Coff]
      val q = Query.ofTable(tbl)
      val q1 = q map (x => x.idNumber) filter (x => x < 3)
      q1.toSeq
    }
    assertEquals("Query map _1 filter of Table + Annotation", List(1, 2), r5.toList)
    val r6 = slickYYV {
      @Entity("COFFEE") case class Coff(@Entity("ID") idNumber: Int, name: String);
      val tbl = Table.getTable[Coff]
      val q = Query.ofTable(tbl)
      val q1 = q map (x => (x.idNumber, x.name)) filter (x => x._1 < 3)
      q1.toSeq
    }
    assertEquals("Query map (_1, _2) filter of Table + Annotation", List((1, "one"), (2, "two")), r6.toList)
    val r7 = slickYYV {
      @Entity("COFFEE") case class Coff(@Entity("ID") idNumber: Int, @Entity("NAME") _2: String);
      val tbl = Table.getTable[Coff]
      val q = Query.ofTable(tbl)
      val q1 = q filter (x => x.idNumber == 3) map (x => (x.idNumber, x._2))
      q1.toSeq
    }
    assertEquals("Query filter == map (_1, _2) of Table + Annotation", List((3, "three")), r7.toList)
    YYUtils.closeSession
  }
  @Test
  def sortTest {
    initSortTable()
    import jdbcTypes._
    import Shallow._
    val r1 = slickYYV {
      case class Coffee(id: Int, name: String);
      val tbl = Table.getTable[Coffee]
      val q = Query.ofTable(tbl)
      val q1 = q map (x => (x.id, x.name)) sortBy (x => x._2)
      q1.toSeq
    }
    assertEquals("Query sort by name of Table", List((2, "one"), (1, "one"), (10, "ten"), (3, "three")), r1.toList)
    val r2 = slickYYV {
      case class Coffee(id: Int, name: String);
      val tbl = Table.getTable[Coffee]
      val q = Query.ofTable(tbl)
      val q1 = q map (x => (x.id, x.name)) sortBy (x => x._1)
      q1.toSeq
    }
    assertEquals("Query sort by id of Table", List((1, "one"), (2, "one"), (3, "three"), (10, "ten")), r2.toList)
    val r3 = slickYYV {
      case class Coffee(id: Int, name: String);
      val tbl = Table.getTable[Coffee]
      val q = Query.ofTable(tbl)
      val q1 = q map (x => (x.id, x.name)) sortBy (x => (x._2, x._1))
      q1.toSeq
    }
    assertEquals("Query sort by (name, id) of Table", List((1, "one"), (2, "one"), (10, "ten"), (3, "three")), r3.toList)
    val r4 = slickYYV {
      case class Coffee(id: Int, name: String);
      val tbl = Table.getTable[Coffee]
      val q = Query.ofTable(tbl)
      val q1 = q map (x => (x.id, x.name)) sortBy (x => (x._2, x._1)) take 2
      q1.toSeq
    }
    assertEquals("Query sort by (name, id) + take of Table", List((1, "one"), (2, "one")), r4.toList)
    val r5 = slickYYV {
      case class Coffee(id: Int, name: String);
      val tbl = Table.getTable[Coffee]
      val q = Query.ofTable(tbl)
      val q1 = q map (x => (x.id, x.name)) sortBy (x => (x._1, x._2)) drop 1
      q1.toSeq
    }
    assertEquals("Query sort by (id, name) + drop of Table", List((2, "one"), (3, "three"), (10, "ten")), r5.toList)

    val r6 = slickYYV {
      case class Coffee(id: Int, name: String);
      val tbl = Table.getTable[Coffee]
      val q = Query.ofTable(tbl)
      val q1 = q.map(x => (x.id, x.name)).sortBy(x => x._1)(Ordering[Int])
      q1.toSeq
    }
    assertEquals("Query sort by id of Table + Ordering", List((1, "one"), (2, "one"), (3, "three"), (10, "ten")), r6.toList)
    val r7 = slickYYV {
      case class Coffee(id: Int, name: String);
      val tbl = Table.getTable[Coffee]
      val q = Query.ofTable(tbl)
      val q1 = q.map(x => (x.id, x.name)).sortBy(x => x._1)(Ordering[Int].reverse)
      q1.toSeq
    }
    assertEquals("Query sort by reverse of id of Table + Ordering", List((10, "ten"), (3, "three"), (2, "one"), (1, "one")), r7.toList)
    val r8 = slickYYV {
      case class Coffee(id: Int, name: String);
      val tbl = Table.getTable[Coffee]
      val q = Query.ofTable(tbl)
      val q1 = q.map(x => (x.id, x.name)).sortBy(x => x._2)(Ordering[String].reverse)
      q1.toSeq
    }
    assertEquals("Query sort by reverse of name of Table + Ordering", List((3, "three"), (10, "ten"), (2, "one"), (1, "one")), r8.toList)
    val r9 = slickYYV {
      case class Coffee(id: Int, name: String);
      val tbl = Table.getTable[Coffee]
      val q = Query.ofTable(tbl)
      val q1 = q.map(x => (x.id, x.name)).sortBy(x => (x._2, x._1))(Ordering[(String, Int)])
      q1.toSeq
    }
    assertEquals("Query sort by (name, id) of Table + Ordering", List((1, "one"), (2, "one"), (10, "ten"), (3, "three")), r9.toList)
    val r10 = slickYYV {
      case class Coffee(id: Int, name: String);
      val tbl = Table.getTable[Coffee]
      val q = Query.ofTable(tbl)
      val q1 = q.map(x => (x.id, x.name)).sortBy(x => (x._2, x._1))(Ordering[(String, Int)].reverse)
      q1.toSeq
    }
    assertEquals("Query sort by reverse of (name, id) of Table + Ordering", List((3, "three"), (10, "ten"), (2, "one"), (1, "one")), r10.toList)
    val r11 = slickYYV {
      case class Coffee(id: Int, name: String);
      val tbl = Table.getTable[Coffee]
      val q = Query.ofTable(tbl)
      val q1 = q.map(x => (x.id, x.name)).sortBy(x => (x._2, x._1))(Ordering.by[(String, Int), String](_._1).reverse)
      q1.toSeq
    }
    assertEquals("Query sort by reverse of name of Table + Ordering", List((3, "three"), (10, "ten"), (2, "one"), (1, "one")), r11.toList)
    val r12 = slickYYV {
      case class Coffee(id: Int, name: String);
      val tbl = Table.getTable[Coffee]
      val q = Query.ofTable(tbl)
      val q1 = q.map(x => (x.id, x.name)).sorted(Ordering.by[(Int, String), String](_._2).reverse)
      q1.toSeq
    }
    assertEquals("Query sorted reverse of name of Table + Ordering", List((3, "three"), (10, "ten"), (2, "one"), (1, "one")), r12.toList)
    val r13 = slickYYV {
      case class Coffee(id: Int, name: String);
      val tbl = Table.getTable[Coffee]
      val q = Query.ofTable(tbl)
      val q1 = q.map(x => (x.id, x.name)).sorted(Ordering.by[(Int, String), (String, Int)](x => (x._2, x._1)).reverse)
      q1.toSeq
    }
    assertEquals("Query sorted by reverse of (name, id) of Table + Ordering", List((3, "three"), (10, "ten"), (2, "one"), (1, "one")), r13.toList)

    YYUtils.closeSession
  }

  @Test
  def forComprehensionTest() {
    initCoffeeTable()
    import jdbcTypes._
    import Shallow._
    val r1 = slickYYV {
      case class Coffee(id: Int, name: String);
      val tbl = Table.getTable[Coffee]
      val q = Query.ofTable(tbl)
      val q1 = for (x <- q) yield x.id
      q1.toSeq
    }
    assertEquals("Query forComprehension map _1 of Virtualized Table", 4, r1.length)
    val r2 = slickYYV {
      case class Coffee(id: Int, name: String);
      val tbl = Table.getTable[Coffee]
      val q = Query.ofTable(tbl)
      val q1 = for (x <- q) yield (x.id, x.name)
      q1.toSeq
    }
    assertEquals("Query forComprehension map (_1, _2) of Table", List((1, "one"), (2, "two"), (3, "three"), (10, "ten")), r2.toList)
    val r3 = slickYYV {
      case class Coffee(id: Int, name: String);
      val tbl = Table.getTable[Coffee]
      val q = Query.ofTable(tbl)
      val q1 = for (x <- q) yield (x.id, if (x.id < 3) "Low" else x.name)
      q1.toSeq
    }
    assertEquals("Query forComprehension map (_1, _2) of Table + if", List((1, "Low"), (2, "Low"), (3, "three"), (10, "ten")), r3.toList)
    val r4 = slickYYV {
      @Entity("COFFEE") case class Coff(@Entity("ID") idNumber: Int, name: String);
      val tbl = Table.getTable[Coff]
      val q = Query.ofTable(tbl)
      val q1 = for (x <- q) yield (x.idNumber, x.name)
      q1.toSeq
    }
    assertEquals("Query forComprehension map (_1, _2) of Table + Annotation", List((1, "one"), (2, "two"), (3, "three"), (10, "ten")), r4.toList)
    val r5 = slickYYV {
      @Entity("COFFEE") case class Coff(@Entity("ID") idNumber: Int, name: String);
      val tbl = Table.getTable[Coff]
      val q = Query.ofTable(tbl)
      val q1 = for (x <- q if x.idNumber < 3) yield x.idNumber
      q1.toSeq
    }
    assertEquals("Query forComprehension map _1 filter of Table + Annotation", List(1, 2), r5.toList)
    val r6 = slickYYV {
      @Entity("COFFEE") case class Coff(@Entity("ID") idNumber: Int, name: String);
      val tbl = Table.getTable[Coff]
      val q = Query.ofTable(tbl)
      val q1 = for (x <- q if x.idNumber < 3) yield (x.idNumber, x.name)
      q1.toSeq
    }
    assertEquals("Query forComprehension map (_1, _2) filter of Table + Annotation", List((1, "one"), (2, "two")), r6.toList)
    val r7 = slickYYV {
      @Entity("COFFEE") case class Coff(@Entity("ID") idNumber: Int, @Entity("NAME") _2: String);
      val tbl = Table.getTable[Coff]
      val q = Query.ofTable(tbl)
      val q1 = for (x <- q if x.idNumber == 3) yield (x.idNumber, x._2)
      q1.toSeq
    }
    assertEquals("Query forComprehension filter == map (_1, _2) of Table + Annotation", List((3, "three")), r7.toList)
    YYUtils.closeSession
  }

  def initCoffeeTable() {
    import scala.slick.driver.H2Driver.simple._

    object Coffee extends Table[(Int, String)]("COFFEE") {
      def id = column[Int]("ID")
      def name = column[String]("NAME")
      def * = id ~ name
    }

    object Test {
      implicit val session = YYUtils.provideSession

      (Coffee.ddl).create

      Coffee.insert((1, "one"))
      Coffee.insert((2, "two"))
      Coffee.insert((3, "three"))
      Coffee.insert((10, "ten"))
    }
    Test
  }
  def initSortTable() {
    import scala.slick.driver.H2Driver.simple._

    object Coffee extends Table[(Int, String)]("COFFEE") {
      def id = column[Int]("ID")
      def name = column[String]("NAME")
      def * = id ~ name
    }

    object Test {
      implicit val session = YYUtils.provideSession

      (Coffee.ddl).create

      Coffee.insert((2, "one"))
      Coffee.insert((1, "one"))
      Coffee.insert((3, "three"))
      Coffee.insert((10, "ten"))
    }
    Test
  }

  def initTable() {
    import scala.slick.driver.H2Driver.simple._
    object Test extends YYSlickCake {
      import TestTable.TableA
      import TestTable.YYTableA
      import TestTable.underlying
      import TestTable.convertTuple2ToTableARow

      implicit val session = YYUtils.provideSession

      (TableA.ddl).create

      TableA.insert((14, 1))
      TableA.insert((18, 1))
      TableA.insert((15, 2))
      TableA.insert((20, 3))
    }
    Test
  }
}
