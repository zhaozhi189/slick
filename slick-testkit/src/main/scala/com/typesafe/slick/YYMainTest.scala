package com.typesafe.slick
//
//import scala.slick.yy.YYTransformers
//import scala.slick.yy.Entity
import scala.tools.reflect.ToolBox
//
object YYMainTest extends App {
  import scala.reflect.runtime.{ universe => ru }
  import ru._
  val tb = runtimeMirror(getClass.getClassLoader).mkToolBox()

  val b = (13, 3)
  val l = List(b)
  class Ext(val f: Int)
  object Ext {
    def unapply(i: (Int, Int)): Option[Ext] = Some(new Ext(i._1 * i._2))
  }

  val patmat = reify {
    //    for ((x, y) <- l) yield (x, y)
    //    for (Ext(x) <- l) yield (x.f)
    b match {
      case (x, y) => true
    }
  }

  println(showRaw(patmat.tree))
  println(patmat)
}
//  
//  val l = List(4, 2, 1)
//  l.sortBy(identity)
//
//  val tree2 = reify {
//    @Entity("COFFEE") case class Coffee(@Entity("ID") id: String, grade: Int)
//    implicit val c = Coffee("2", 3)
//    case class Supplier(sid: Long)
//  }.tree
//  import scala.slick.yy.YYColumn
//  import scala.slick.yy.YYTable
//  import scala.slick.yy.TestTable.TableA
//  import scala.slick.yy.YYTableARow
//  val ClassVirtualization = {
//    new {
//      val universe: ru.type = ru
//      val mirror = scala.reflect.runtime.universe.runtimeMirror(getClass.getClassLoader())
//    } with YYTransformers
//  }.ClassVirtualization
//  val tree3 = reify {
//    class YYTableA(val table: TableA) extends YYTable[YYTableARow] {
//
//      def id = YYColumn(table.id)
//      def grade = YYColumn(table.grade)
//      override def toString = "YYTableA"
//    }
//  }.tree
//  val vtree2 = ClassVirtualization(tree2)
//  val tree4 = reify {
//    trait Rep[T] {
//      def underlying: T
//    }
//    trait Table[T]
//    class A
//    implicit object A extends A
//    class B(val a: A)
//    implicit object implicitB extends B(A)
//    implicit def convB(x: Rep[B]): Table[B] = x.asInstanceOf[Table[B]]
//	implicit def convC(x: Rep[A]): B = new B(x.underlying.asInstanceOf[A])
//  }.tree
//  def main(args: Array[String]) {
//    //    val tree = tb.parse("@asghar case class A(@hello id: String)")
//    //    val ttree = ClassVirtualization(tree)
//    //    println(showRaw(tree2))
//    println(showRaw(tree4))
//    //    println(ttree)
//    //    println(typeToTable(typeOf[Coffee]))
//    //    println(showRaw(vtree2))
//    //    println(vtree2)
//    //    println(showRaw(tree3))
//  }
//}