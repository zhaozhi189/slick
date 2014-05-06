package scala.slick.testutil

import scala.slick.driver.PostgresDriver

trait CustomPostgresDriver extends PostgresDriver {

  override val Implicit = new ImplicitsPlus {}
  override val simple = new SimpleQLPlus {}

  trait ImplicitsPlus extends Implicits

  trait SimpleQLPlus extends SimpleQL with ImplicitsPlus
}

object CustomPostgresDriver extends CustomPostgresDriver
