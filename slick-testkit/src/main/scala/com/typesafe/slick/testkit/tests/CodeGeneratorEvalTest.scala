package com.typesafe.slick.testkit.tests

import org.junit.Assert._
import scala.slick.model.codegen._
import com.typesafe.slick.testkit.util.{JdbcTestDB, ExternalJdbcTestDB, TestkitTest}
import scala.reflect.runtime.universe._

class CodeGeneratorEvalTest extends TestkitTest[JdbcTestDB] {
  import tdb.profile.simple._

  def test { ifCap(jcap.createModel){
    case class Category(id: Int, name: String)
    class Categories(tag: Tag) extends Table[Category](tag, "categories") {
      def id = column[Int]("id", O.PrimaryKey, O.AutoInc)
      def name = column[String]("name")
      def * = (id, name) <> (Category.tupled,Category.unapply)
      def idx = index("IDX_NAME",name)
    }
    val categories = TableQuery[Categories]

    class Posts(tag: Tag) extends Table[(Int, String, Option[Int])](tag, "posts") {
      def id = column[Int]("id")
      def title = column[String]("title")
      def category = column[Option[Int]]("category")
      def * = (id, title, category)
      def categoryFK = foreignKey("category_fk", category, categories)(_.id)
    }
    val posts = TableQuery[Posts]


    class TypeTest(tag: Tag) extends Table[(Boolean,Byte,Short,Int,Long,Float,Double,String,java.sql.Date,java.sql.Time,java.sql.Timestamp,java.sql.Blob)](tag, "TYPE_TEST") {
      def Boolean = column[Boolean]("Boolean",O.Default(true))
      def Byte = column[Byte]("Byte")
      def Short = column[Short]("Short")
      def Int = column[Int]("Int",O.Default(5))
      def Long = column[Long]("Long",O.Default(5L))
      //def java_math_BigInteger = column[java.math.BigInteger]("java_math_BigInteger")
      def Float = column[Float]("Float",O.Default(9.999F))
      def Double = column[Double]("Double",O.Default(9.999))
      //def java_math_BigDecimal = column[java.math.BigDecimal]("java_math_BigDecimal")
      def String = column[String]("String",O.Default("someDefaultString"))
      def java_sql_Date = column[java.sql.Date]("java_sql_Date")
      def java_sql_Time = column[java.sql.Time]("java_sql_Time")
      def java_sql_Timestamp = column[java.sql.Timestamp]("java_sql_Timestamp")
      def java_sql_Blob = column[java.sql.Blob]("java_sql_Blob")
      def java_sql_Clob = column[java.sql.Clob]("java_sql_Clob")
      def * = (Boolean,Byte,Short,Int,Long,Float,Double,String,java_sql_Date,java_sql_Time,java_sql_Timestamp,java_sql_Blob)
      def pk = primaryKey("PK", (Int,Long))
    }
    val typeTest = TableQuery[TypeTest]

    val ddl = posts.ddl ++ categories.ddl ++ typeTest.ddl
    //println("-"*80)
    //println(ddl.createStatements.mkString("\n"))
    //println("=====>")
    try{
      ddl.create
    } catch {
      case e:Throwable => println(ddl.createStatements.mkString("\n")); throw e
    }
    val model = tdb.profile.createModel
    //println(ddl.dropStatements.toList).mkString("\n"))
    try{
      ddl.drop
    } catch {
      case e:Throwable => println(ddl.dropStatements.mkString("\n")); throw e
    }

    new TreeGenerator(model){
      val driver = parseTerm(tdb.profile.getClass.toString.drop(6).dropRight(1))
      val (user,password) = tdb match {
        case t:ExternalJdbcTestDB => (q"${t.user}",q"${t.password}")
        case _ => (q"null",q"null")
      }
      val codeWithImport = q"""
        import ${driver}.simple._
        object Tables{
          trait Foo
          trait Bar[T]
          ..$codeTrees
        }
        val ddl = Tables.Posts.ddl ++ Tables.Categories.ddl ++ Tables.TypeTest.ddl
        //println(ddl.createStatements.mkString("\n"))

        val db = Database.forURL(${tdb.url}, driver=${tdb.jdbcDriver}, user=$user, password=$password )
        db.withSession{ implicit session =>
          try{
            ddl.create
            Tables.Categories.insert( Tables.CategoriesRow(1,"cat") )
            Tables.Posts.insertAll(
              Tables.PostsRow(1,"post 1",Some(1)),
              Tables.PostsRow(2,"post 2",Some(1)),
              Tables.PostsRow(3,"post 3",Some(1))
            )
            Tables.Categories.insert( Tables.CategoriesRow(2,"cat") )
            ( Tables.Posts.length.run, Tables.Posts.filter(_.title =!= "post 1").map(_.title).run.toList )
          } catch {
            case e:Throwable => println(ddl.createStatements.mkString("\n")); throw e
          }
        }
      """
      import scala.reflect.runtime.{currentMirror=>cm}
      import scala.tools.reflect._
      try{
          assertEquals((3,List("post 2","post 3")), cm.mkToolBox().eval(codeWithImport))
          //println(Unparser.unparse(codeWithImport))
      } catch {
        case e:Throwable => {println(model);println(Unparser.unparse(codeWithImport));throw e}
      }
      override def Table = new Table(_){
        override def autoIncLastAsOption = false
        override def TableClass = new TableClass{
          override def parents = Seq(tq"Foo",tq"Bar[Int]")
        }
      }
    }    
  }}
}
