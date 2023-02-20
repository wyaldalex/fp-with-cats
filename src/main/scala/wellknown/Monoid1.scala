package wellknown

import cats.Monoid
import cats.implicits.catsSyntaxSemigroup
import cats._
import cats.implicits._

object Monoid1 extends App {

  case class Speed(metersPerSecond: Double) {
    def kilometersPerSec: Double = metersPerSecond / 1000.0
    def milesPerSec: Double = metersPerSecond / 1609.34
  }

  object Speed {
    def addSpeeds(s1: Speed, s2: Speed): Speed = {
      Speed(s1.metersPerSecond + s2.metersPerSecond)
    }

//    implicit val monoidSpeed: Monoid[Speed] = new Monoid[Speed] {
//      override def empty: Speed = Speed(0)
//
//      override def combine(x: Speed, y: Speed): Speed = addSpeeds(x,y)
//    }
    //shorthand
    implicit val monoidSpeed: Monoid[Speed] = Monoid.instance(Speed(0), addSpeeds)
    implicit val eqSpeed: Eq[Speed] =  Eq.fromUniversalEquals
  }

  val s1 = Speed(2121)
  val s2 = Speed(121)
  val combineResult = Monoid[Speed].combine(s1,s2)
  println(combineResult)
  println(s"Another esoteric shorthand ${s1 |+| s2}" )

  //more magic
  println(List(s1,s2,Speed(3000)).combineAll)
  println(Monoid[Speed].isEmpty(Speed(100)))

}

object ListGenericMonoidExercise extends App {

  def addLists[A] (l1: List[A], l2: List[A]) : List[A] = {
    l1 ++ l2
  }

  implicit val listMonoidInt: Monoid[List[Int]] = Monoid.instance(List(), addLists)

  println(Monoid[List[Int]].combine(List(12,2),List(3,4)))
}
