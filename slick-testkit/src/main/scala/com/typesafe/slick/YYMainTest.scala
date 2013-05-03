package com.typesafe.slick

import scala.slick.yy.YYTransformers
import scala.slick.yy.Entity
import scala.tools.reflect.ToolBox

object YYMainTest {
  import scala.reflect.runtime.{ universe => ru }
  import ru._
  val tb = runtimeMirror(getClass.getClassLoader).mkToolBox()

  val tree2 = reify {
    @Entity("COFFEE") case class Coffee(@Entity("ID") id: String, grade: Int)
    val c = Coffee("2", 3)
    case class Supplier(sid: Long)
  }.tree
  import scala.slick.yy.YYColumn
  import scala.slick.yy.YYTable
  import scala.slick.yy.TestTable.TableA
  import scala.slick.yy.YYTableARow
  val ClassVirtualization = {
    new {
      val universe: ru.type = ru
      val mirror = scala.reflect.runtime.universe.runtimeMirror(getClass.getClassLoader())
    } with YYTransformers
  }.ClassVirtualization
  val tree3 = reify {
    class YYTableA(val table: TableA) extends YYTable[YYTableARow] {

      def id = YYColumn(table.id)
      def grade = YYColumn(table.grade)
      override def toString = "YYTableA"
    }
  }.tree
  val vtree2 = ClassVirtualization(tree2)
  def main(args: Array[String]) {
    //    val tree = tb.parse("@asghar case class A(@hello id: String)")
    //    val ttree = ClassVirtualization(tree)
    println(showRaw(tree2))
    //    println(ttree)
    //    println(typeToTable(typeOf[Coffee]))
    //    println(showRaw(vtree2))
    println(vtree2)
    //    println(showRaw(tree3))
  }
}