package scala.slick.test.compile

import scala.slick.driver.H2Driver.simple._
import com.typesafe.slick.testkit.util.ShouldNotTypecheck

class IllegalExecutorTest {
  class T(tag: Tag) extends Table[(Int, Option[Int])](tag, "t2") {
    def a = column[Int]("a")
    def b = column[Option[Int]]("b")
    def * = (a, b)
  }
  val ts = TableQuery[T]

  def cannotRunNestedQuery = ShouldNotTypecheck("""
      ts.groupBy(_.a).run
    """, "value run is not a member of .*")
}
