package wellknown

import cats._
import cats.implicits._

import java.util.Date

object ShowTest extends App {
  val showInt = Show.apply[Int]
  println(showInt.show(12121))

  val intAsString = showInt.show(12121)
  println(intAsString)

  implicit val dateShow: Show[Date] =
    Show.show(date => s"${date.getTime} ms since the epoch.")
  val dateAsString = dateShow.show(new Date(2023,2,3))
  println(dateAsString)


  case class Person(name: String, lastName: String, age: Int)
  implicit val personShow: Show[Person] =
    Show.show(person => s"${person.name}-*-${person.lastName}-*-${person.age}")
  val personStr = personShow.show(Person("John","Locke",50))
  println(personStr)

}
