package scala.slick.test.compile

import com.typesafe.slick.testkit.util.ShouldNotTypecheck

class NegativeCompilationTests {
  def extendingFinalClass = ShouldNotTypecheck("""
      class Foo extends String
    """, ".*illegal inheritance.*final.*")
}
