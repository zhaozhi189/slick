package scala.slick.test.jdbc

import scala.language.{ reflectiveCalls, implicitConversions }
import org.junit.Test
import org.junit.Assert._
import scala.slick.testutil._
import scala.reflect.runtime.universe.TypeTag
import scala.reflect.ClassTag
import com.typesafe.slick.testkit.util.TestDB
import slick.jdbc.StaticQuery.interpolation
import scala.slick.SlickException
import scala.slick.yy._
import ch.qos.logback.core.pattern.util.AsIsEscapeUtil

object ShallowTest extends DBTestObject(TestDBs.H2Mem, TestDBs.H2Disk, TestDBs.HsqldbMem, TestDBs.HsqldbDisk, TestDBs.SQLiteMem, TestDBs.SQLiteDisk, TestDBs.DerbyMem, TestDBs.DerbyDisk)

@Entity("COFFEES") case class Coffee(@Entity("COF_NAME") name: String, sales: Int)
//class Foo[T]( val q : Queryable[T] )
//
//@table(name="COFFEES")
//case class Coffee(
//  @column(name="COF_NAME")
//  name : String
//  ,@column // <- assumes "SALES" automatically
//  sales : Int
//  ,@column
//  flavor : Option[String]
//)

class ShallowTest(val tdb: TestDB) extends DBTest {
  implicit val testDriver = tdb.driver
  implicit val testSession = tdb.createDB().createSession

  object Singleton {
    import Shallow._
    val q = shallow {
      Queryable[Coffee].toSeq
    }
    val q1 = shallow {
      Queryable[Coffee].map(_.sales + 5).toSeq
    }
    object Singleton {
      val q = shallow {
        Queryable[Coffee].toSeq
      }
      val q1 = shallow {
        Queryable[Coffee].map(_.sales + 5).toSeq
      }
      object Singleton {
        val q = shallow {
          Queryable[Coffee].toSeq
        }
        val q1 = shallow {
          Queryable[Coffee].map(_.sales + 5).toSeq
        }
      }
    }
  }

  object TestingTools {
    //    def enableAssertQuery[T:TypeTag:ClassTag]( q:Queryable[T] ) = new{
    //      def assertQuery( matcher : ast.Node => Unit ) = {
    //        //backend.dump(q)
    //        println( backend.toSql(q,threadLocalSession) )
    //        println( backend.result(q,threadLocalSession) )
    //        try{
    //          matcher( backend.toQuery( q )._2.node : @unchecked ) : @unchecked
    //          print(".")
    //        } catch {
    //          case e:MatchError => {
    //            println("F")
    //            println("")
    //            backend.dump(q)
    //            assert(false,"did not match")
    //          }
    //        }
    //      }
    //    }
    //    object TableName{
    //      def unapply( t:ast.TableNode ) = {
    //        val name = t.tableName
    //        Some(name)
    //      }
    //    }
    //    object ColumnName{
    //      def unapply( t:ast.Symbol ) = t match {
    //        case ast.FieldSymbol( name ) =>
    //          /*case RawNamedColumn( name, _, _ ) =>*/
    //          Some(name)
    //      }
    //    }
    //    def fail(msg:String = ""){
    //      println("F")
    //      throw new Exception(msg)
    //    }
    //    def fail : Unit = fail()
    //    def success{ print(".") }
    //    def assertEqualMultiSet[T]( lhs:scala.collection.Traversable[T], rhs:scala.collection.Traversable[T] ) = assertEquals( rhs.groupBy(x=>x), lhs.groupBy(x=>x) )
    //    def assertMatch[T:TypeTag:ClassTag]( queryable:Queryable[T], expected: Traversable[T] ) = assertEqualMultiSet( backend.result(queryable,threadLocalSession), expected)
    //    def assertNotEqualMultiSet[T]( lhs:scala.collection.Traversable[T], rhs:scala.collection.Traversable[T] ) = assertEquals( lhs.groupBy(x=>x), rhs.groupBy(x=>x) )
    //    def assertNoMatch[T:TypeTag:ClassTag]( queryable:Queryable[T], expected: Traversable[T] ) = try{
    //      assertEqualMultiSet( backend.result(queryable,threadLocalSession), expected)
    //    } catch {
    //      case e:AssertionError => 
    //      case e:Throwable => throw e
    //    }
    //    def assertMatchOrdered[T:TypeTag:ClassTag]( queryable:Queryable[T], expected: Traversable[T] ) = assertEquals( expected, backend.result(queryable,threadLocalSession) )
    def assertMatchOrdered[T <: Product, T1 <: Product](expected: Vector[T], actual: Seq[T1]) =
      assertMatchCaseClass(expected, actual)
    def assertMatchCaseClass[T <: Product, T1 <: Product](expected: Vector[T], actual: Seq[T1]) = {
      //      def convertToTuple2(src: Product): (String, Int) = (src.productElement(0).asInstanceOf[String], src.productElement(1).asInstanceOf[Int])
      //      val expTuple2 = expected map convertToTuple2
      //      val actTuple2 = actual map convertToTuple2
      //      assertEquals(expTuple2, actTuple2.toVector)
      assertEquals(expected.toList, actual.toList)
    }
    def assertMatch[T](expected: Vector[T], actual: Seq[T]) = {
      assertEquals(expected, actual.toVector)
    }

  }

  object SingletonInClass {
    //    val qoo = Queryable[Coffee]
    //    val q1 = qoo.map( _.sales + 5 )
  }

  @Test def test() {
    //    implicit var stringOptionOrdering: scala.math.Ordering[Option[String]] = initialStringOptionOrdering

    import TestingTools._

    val coffees_data = Vector(
      ("Colombian", 1),
      ("French_Roast", 2),
      ("Espresso", 3),
      ("Espresso", 4),
      ("Colombian_Decaf", 1),
      ("Colombian_Decaf", 3),
      ("French_Roast_Decaf", 5))

    {
      // create test table
      sqlu"create table COFFEES(COF_NAME varchar(255), SALES int)".execute
      (for {
        (name, sales) <- coffees_data
      } yield sqlu"insert into COFFEES values ($name, $sales)".first).sum

      // FIXME: reflective typecheck failed:  backend.result(Queryable[Coffee].map(x=>x))

      // setup query and equivalent inMemory data structure
      val inMem = coffees_data.map { case (name, sales) => Coffee(name, sales) }
      import Shallow._
      val query = shallow {
        Queryable[Coffee].toSeq
      }

      // test framework sanity checks
      //      assertNoMatch(query, inMem ++ inMem)
      //      assertNoMatch(query, List())

      // fetch whole table
      assertMatchCaseClass(inMem, query)
      // FIXME: make this test less artificial
      class MyQuerycollection {
        def findUserByName(name: String) = shallow {
          Queryable[Coffee].filter(_.name == name).toSeq
        }
      }
      val qc = new MyQuerycollection
      qc.findUserByName("some value")

      // test singleton object
      assertMatch(
        inMem.map(_.sales + 5),
        Singleton.q1)

      // test singleton in package
      assertMatch(
        inMem.map(_.sales + 5),
        Singleton.q1)

      // test singleton in singleton in package
      assertMatch(
        inMem.map(_.sales + 5),
        Singleton.Singleton.q1)

      // test singleton in singleton in singleton in package
      assertMatch(
        inMem.map(_.sales + 5),
        Singleton.Singleton.Singleton.q1)

      /*
      // test singleton in class (not supported (yet?))
      try {
        assertMatch(
          SingletonInClass.q1,
          inMem.map(_.sales + 5))
        fail()
      } catch {
        case _: SlickException =>
      }
      */

      // simple map
      assertMatch(
        inMem.map((_: Coffee).sales + 5),
        shallow {
          Queryable[Coffee].map(_.sales + 5).toSeq
        })

      /*
      // left-hand-side coming from attribute
      val foo = new Foo(query)
      assertMatch(
        foo.q.map((_: Coffee).sales + 5),
        inMem.map((_: Coffee).sales + 5))
      */

      // map with string concatenation
      assertMatch(
        inMem.map(_.name + "."),
        shallow {
          Queryable[Coffee].map(_.name + ".").toSeq
        })

      // filter with more complex condition
      assertMatchCaseClass(
        inMem.filter(c => c.sales > 5 || "Chris" == c.name),
        shallow {
          Queryable[Coffee].filter(c => c.sales > 5 || "Chris" == c.name).toSeq
        })

      // type annotations FIXME canBuildFrom
      assertMatch(
        inMem.map((_: Coffee).name: String),
        shallow {
          Queryable[Coffee].map[String](_.name: String).toSeq
        })

      // chaining
      assertMatch(
        inMem.map(_.name).filter(_ == ""),
        shallow {
          Queryable[Coffee].map(_.name).filter(_ == "").toSeq
        })

      // referenced values are inlined as constants using reflection
      val o = 2 + 3
      assertMatchCaseClass(
        inMem.filter(_.sales > o),
        shallow {
          Queryable[Coffee].filter(_.sales > o).toSeq
        })

      // nesting (not supported yet: query.map(e1 => query.map(e2=>e1))) 
      assertMatchCaseClass(
        inMem.flatMap(e1 => inMem.map(e2 => e1)),
        shallow {
          Queryable[Coffee].flatMap(e1 => Queryable[Coffee].map(e2 => e1)).toSeq
        })

      // query scope
      {
        val inMemResult = inMem.filter(_.sales > 5)
        List(
          shallow {
            Queryable[Coffee].filter(_.sales > 5).toSeq
          },
          // Queryable( query.filter( _.sales > 5 ) ),
          shallow {
            val foo = Queryable[Coffee]
            val bar = foo.filter(_.sales > 5)
            bar.toSeq
          }).foreach {
            query_ => assertMatchCaseClass(inMemResult, query_)
          }
      }

      // comprehension with map
      assertMatch(
        for (c <- inMem) yield c.name,
        shallow {
          (for (c <- Queryable[Coffee]) yield c.name).toSeq
        })

      // nesting with flatMap
      {
        val inMemResult = for (o <- inMem; i <- inMem) yield i.name
        List(
          shallow {
            Queryable[Coffee].flatMap(o => Queryable[Coffee].map(i => i.name)).toSeq
          },
          // Queryable(for (o <- query; i <- query) yield i.name), 
          shallow {
            (for (o <- Queryable[Coffee]; i <- Queryable[Coffee]) yield i.name).toSeq
          }).foreach {
            query_ => assertMatch(inMemResult, query_)
          }
      }

      assertMatchCaseClass(
        inMem.flatMap(e1 => inMem.map(e2 => e1).map(e2 => e1)),
        shallow {
          Queryable[Coffee].flatMap(e1 => Queryable[Coffee].map(e2 => e1).map(e2 => e1)).toSeq
        })

      // nesting with outer macro reference
      {
        val inMemResult = for (o <- inMem; i <- inMem) yield o.name
        List(
          shallow {
            Queryable[Coffee].flatMap(o => Queryable[Coffee].map(i => o.name)).toSeq
          },
          // Queryable(for (o <- query; i <- query) yield o.name),
          shallow {
            (for (o <- Queryable[Coffee]; i <- Queryable[Coffee]) yield o.name).toSeq
          }).foreach {
            query_ => assertMatch(inMemResult, query_)
          }
      }

      // nesting with chaining / comprehension with cartesian product and if
      {
        val inMemResult = for (o <- inMem; i <- inMem; if i.sales == o.sales) yield i.name
        List(
          shallow {
            Queryable[Coffee].flatMap(o => Queryable[Coffee].filter(i => i.sales == o.sales).map(i => i.name)).toSeq
          },
          // Queryable(for (o <- query; i <- query; if i.sales == o.sales) yield i.name),
          shallow {
            (for (o <- Queryable[Coffee]; i <- Queryable[Coffee]; if i.sales == o.sales) yield i.name).toSeq
          }).foreach {
            query_ => assertMatch(inMemResult, query_)
          }
      }

      // tuples
      assertMatch(
        inMem.map(c => (c.name, c.sales)),
        shallow {
          Queryable[Coffee].map(c => (c.name, c.sales)).toSeq
        })

      // nested structures (here tuples and case classes)
      //      assertMatch(
      //        inMem.map(c => (c.name, c.sales, c)),
      //        shallow {
      //          Queryable[Coffee].map(c => (c.name, c.sales, c)).toSeq
      //        })
      // length
      assertEquals(inMem.length, shallow {
        Query(Queryable[Coffee].length).first
      })

      assertMatchCaseClass(
        inMem.map(c => c),
        shallow {
          Queryable[Coffee].map(c => c).toSeq
        })

      assertMatch(for (v1 <- inMem; v2 <- inMem; if !(v1.name == v2.name)) yield (v1.name, v2.name),
        shallow {
          (for (v1 <- Queryable[Coffee]; v2 <- Queryable[Coffee]; if !(v1.name == v2.name)) yield (v1.name, v2.name)).toSeq
        })

      assertMatchCaseClass(inMem.take(2),
        shallow { Queryable[Coffee].take(2).toSeq })

      assertMatchCaseClass(inMem.drop(2),
        shallow { Queryable[Coffee].drop(2).toSeq })

      assertMatchOrdered(inMem.sortBy(_.name),
        shallow { Queryable[Coffee].sortBy(_.name).toSeq })

      assertMatchOrdered(inMem.sortBy(c => (c.name, c.sales)),
        shallow { Queryable[Coffee].sortBy(c => (c.name, c.sales)).toSeq })

      assertMatchOrdered(inMem.sortBy(c => c.name)(Ordering[String].reverse),
        shallow { Queryable[Coffee].sortBy(c => c.name)(Ordering[String].reverse).toSeq })

      assertMatchOrdered(inMem.sortBy(c => (c.name, c.sales))(Ordering.Tuple2(Ordering[String], Ordering[Int].reverse)),
        shallow { Queryable[Coffee].sortBy(c => (c.name, c.sales))(Ordering.Tuple2(Ordering[String], Ordering[Int].reverse)).toSeq })

      /*


      def nullOrdering(x: Int, y: Int) = new scala.math.Ordering[Option[String]] {
        def compare(a: Option[String], b: Option[String]) = {
          if (a == None && b == None) 0
          else if (a == None) x * -1
          else if (b == None) x * 1
          else y * (a.get compare b.get)
        }
      }

      stringOptionOrdering = nullOrdering(-1, 1)
      assertMatchOrdered(
        query.sortBy(c => (nonesLast(c.flavor), c.name)), inMem.sortBy(c => (c.flavor, c.name)))

      stringOptionOrdering = nullOrdering(1, 1)
      assertMatchOrdered(
        query.sortBy(c => (nonesFirst(c.flavor), c.name)), inMem.sortBy(c => (c.flavor, c.name)))

      stringOptionOrdering = nullOrdering(-1, -1)
      assertMatchOrdered(
        query.sortBy(c => (nonesLast(reversed(c.flavor)), c.name)), inMem.sortBy(c => (c.flavor, c.name)))

      stringOptionOrdering = nullOrdering(1, -1)
      assertMatchOrdered(
        query.sortBy(c => (nonesFirst(reversed(c.flavor)), c.name)), inMem.sortBy(c => (c.flavor, c.name)))

      stringOptionOrdering = initialStringOptionOrdering
      assertMatchOrdered(
        query.sortBy(c => (
          c.name, reversed(c.sales), reversed(c.flavor))), inMem.sortBy(c => (
          c.name, reversed(c.sales), reversed(c.flavor))))
      *
      */
    }
  }
}
