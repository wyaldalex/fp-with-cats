package wellknown

import cats._
import cats.implicits._

object CatsEither extends App {
  def countsPositives(nums: List[Int])= {
    nums.foldLeft(0.asRight[String]) { (accumulator, num) =>
      if(num >0) {
        accumulator.map(_ + 1)
      } else {
        Left("Negative. Stopping!")
      }
    }
  }

  println(countsPositives(List(10,21,13,131,31)))
}
