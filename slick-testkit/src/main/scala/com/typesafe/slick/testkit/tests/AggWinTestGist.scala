package com.typesafe.slick.testkit.tests

import org.junit.Test
import org.junit.Assert._
import scala.slick.ast.Library.SqlAggregateFunction
import scala.slick.lifted.{OptionMapperDSL, AbstractTable}
import scala.slick.lifted.AggWinFuncSupport._
import scala.slick.jdbc.JdbcType

/**
 * SELECT depname, empno, salary, avg(salary), rank() OVER (PARTITION BY depname ORDER BY salary DESC) FROM empsalary
 * --------------
 * empsalary.groupBy(r => 1).flatMap{ case (_, group) =>
 *    group.map( e => (e.depname, e.empno, e.salary,
 *                group.sortBy(_.salary.desc).map(_.salary).avg,
 *                group.sortBy(_.salary.desc).groupBy(_.depname).rank()
 *             ))
 * }
 */
object AggWinTestGist {
  import scala.language.higherKinds
  import scala.language.implicitConversions
  import scala.slick.driver.PostgresDriver.simple._

  val db = Database.forURL(url = "jdbc:postgresql://localhost/test?user=test", driver = "org.postgresql.Driver")

  case class EmpSalary(depName: String, empNo: String, salary: Double)

  class EmpSalaryTable(tag: Tag) extends Table[EmpSalary](tag, "emp_salary") {
    def depName = column[String]("dep_name")
    def empNo = column[String]("emp_no")
    def salary = column[Double]("salary")

    def * = (depName, empNo, salary) <> (EmpSalary.tupled, EmpSalary.unapply)
  }
  val Tabs = TableQuery[EmpSalaryTable]

  //////////////////////////////////////////////////////////
  val Avg = new SqlAggregateFunction("avg")
  val Corr = new SqlAggregateFunction("corr")
  trait AggregateFunctions[TT <: AbstractTable[_]] extends AggregateSupport[TT] {
    //TODO how to resolve option type of column?
    def avg[P](x: (TT => Column[P]))(implicit tm: JdbcType[P]) =
      mkColumn[P](Avg, List(x(theTable).toNode))
    def corr[P1, P2, R](x: (TT => Column[P1]), y: (TT => Column[P2]))(
        implicit tm: JdbcType[Double], om: OptionMapperDSL.arg[Double, P1]#arg[Double, P2]#to[Double, R]) =
      mkColumn[R](Corr, List(x(theTable).toNode, y(theTable).toNode))(om.liftedType)
  }

  val Rank = new SqlAggregateFunction("rank")
  trait WindowFunctions[TT <: AbstractTable[_]] extends AggregateSupport[TT] {
    def rank()(implicit tm: JdbcType[Int]) = mkColumn[Int](Rank)
  }

  ///
  implicit def windowFunctionExtensionMethods[K, T, G, P <: AbstractTable[_], U, C[_]](q: Query[(G, Query[P, U, C]), (T, Query[P, U, C]), C]) =
    new WindowFunctionExtensionMethodsBase(q) with WindowFunctions[P] with AggregateFunctions[P]
  implicit def aggregateFunctionExtensionMethods[E <: AbstractTable[_], U, C[_]](q: Query[E, U, C]) =
    new AggregateFunctionExtensionMethodsBase(q) with AggregateFunctions[E]

  //////////////////////////////////////////////////////////
  @Test
  def main(args: Array[String]): Unit = {
    db withSession { implicit session: Session =>
      Tabs.ddl.drop;
      Tabs.ddl.create
      Tabs ++= Seq(
        EmpSalary("foo", "bar",  205.3d),
        EmpSalary("foo", "quux", 321.9d),
        EmpSalary("baz", "quux", 411.05d),
        EmpSalary("az", "quux", 355.11d)
      )

      println("============== testing window function support =============")

      val q = Tabs.sortBy(_.depName).groupBy(r => 1).flatMap {
        case (_, group) =>
          group.map(e => (e.depName, e.empNo, e.salary,
            group.sortBy(_.salary.desc).groupBy(g => 1).withFrame(e => (e.ROWS, e.boundPreceding(7), Some(e.currentRow))).avg(_.salary),
            group.sortBy(_.salary.desc).groupBy(_.depName).rank(),
            group.sortBy(_.salary.desc).groupBy(g => 1).corr(_.salary, _.salary)
          ))
      }
      println(q.selectStatement)

      val q1 = Tabs.groupBy(_.depName).map {
        case (depName, group) => (depName, group.distinct.avg(_.salary))
      }
      println(q1.selectStatement)
    }
  }
}
