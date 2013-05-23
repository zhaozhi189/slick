package scala.slick.yy

import scala.{ Int => SInt }
import scala.language.implicitConversions
import scala.slick.lifted.Case
import scala.slick.ast.BaseTypedType
import scala.slick.lifted.AbstractTable
import scala.slick.driver.H2Driver
import scala.slick.jdbc.JdbcBackend
import scala.slick.driver.JdbcDriver
import scala.slick.SlickException
import scala.slick.profile.BasicDriver
import scala.slick.driver.JdbcProfile

trait YYSlickCake extends YYSlickCakeTuples with YYSlickLowPriorityImplicits {
  type CakeRep[T] = YYRep[T]
  type Column[T] = YYColumn[T]
  type Table[T] = YYTable[T]
  type Query[T] = YYQuery[T]
  type JoinQuery[T1, T2] = YYJoinQuery[T1, T2]
  type Int = YYColumn[SInt]
  type Long = YYColumn[scala.Long]
  type Double = YYColumn[scala.Double]
  type String = YYColumn[Predef.String]
  type Boolean = YYColumn[scala.Boolean]
  type ColumnOps[T] = YYColumn[T]
  type Invoker[T] = scala.slick.yy.Shallow.Invoker[T]

  //order stuff
  type Ordering[T] = YYOrdering[T]
  val Ordering = YYOrdering
  val String = YYOrdering.String
  val Int = YYOrdering.Int

  implicit def fixClosureContraVariance[T, U <: YYRep[T], S](x: U => S) =
    x.asInstanceOf[YYRep[T] => S]

  implicit def yyRepIntToColumnOps(x: CakeRep[SInt]): Int =
    x.asInstanceOf[Int]
  implicit def yyRepLongToColumnOps(x: CakeRep[scala.Long]): Long =
    x.asInstanceOf[Long]
  implicit def yyRepStringToColumnOps(x: CakeRep[Predef.String]): String =
    x.asInstanceOf[String]
  implicit def yyRepDoubleToColumnOps(x: CakeRep[scala.Double]): Double =
    x.asInstanceOf[Double]

  object Queryable {
    def apply[T](implicit t: YYTable[T]): Query[T] = YYQuery.apply(t)
  }

  object Query {
    def apply[T](v: YYRep[T]): YYQuery[T] = YYQuery.apply(v)
    def ofTable[T](t: YYTable[T]): YYQuery[T] = YYQuery.apply(t)
  }

  object Shallow {
    object ColumnOps {
      def apply[T](value: YYColumn[T]): YYColumn[T] = value
    }
    def stringWrapper(value: String): String = value
  }

  def intWrapper(value: Int): Int = value

  //  def augmentString(value: String): String = value

  def __ifThenElse[T: BaseTypedType](c: => Boolean, t: Column[T], e: Column[T]): Column[T] = {
    val condition = Case.If(c.underlying)
    val _then = condition.Then[T](t.underlying)
    val _else = _then.Else(e.underlying)
    YYColumn(_else)
  }

  def __equals[T](t: Column[T], e: Column[T]) = t === e

  object Table {
    def getTable[S](implicit mapping: Table[S]): Table[S] = mapping
  }

}

trait YYSlickLowPriorityImplicits {
  // These two implicits are needed for the cake to be type chacked!

  // Type of this one is JdbcProfile and not JdbcDriver in order to make it lower priority in comparison with the implicit driver which will
  // provided by the user. If type of this one is JdbcDriver, we would get 'ambiguous implicit' error. 
  implicit def dummyDriver: JdbcProfile = throw new SlickException("You forgot to provide appropriate implicit jdbc driver for YY block!")
  // The reason of generality of this type is the same as the above one.
  implicit def dummySession: JdbcBackend#Session = throw new SlickException("You forgot to provide implicit session for YY block!")
}
