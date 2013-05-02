package com.typesafe.slick

import scala.slick.yy.ClassVirtualization
import scala.slick.yy.Entity
import scala.tools.reflect.ToolBox

object YYMainTest {
  import scala.reflect.runtime.universe
  import universe._
  val tb = runtimeMirror(getClass.getClassLoader).mkToolBox()

  val tree2 = reify {
    @Entity("COFFEE") case class Coffee(@Entity("ID") id: String, grade: Int)
    val c = Coffee("2", 3)
    case class Supplier(sid: Long)
  }.tree
  val vtree2 = ClassVirtualization(tree2)
  def main(args: Array[String]) {
    //    val tree = tb.parse("@asghar case class A(@hello id: String)")
    //    val ttree = ClassVirtualization(tree)
    //    println(showRaw(tree2))
    //    println(ttree)
    //    println(typeToTable(typeOf[Coffee]))
    //    println(showRaw(vtree2))
    println(vtree2)
  }
}