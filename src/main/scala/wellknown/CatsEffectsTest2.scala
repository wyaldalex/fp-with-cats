package wellknown

import cats._
import cats.effect._
import cats.implicits._
import cats.effect.implicits._
import cats.effect.unsafe.implicits.global

import scala.io.StdIn

object CatsEffectsTest2 {

  object Console {
    def putStrLn(s: String): IO[Unit] = IO(println(s))
    def readLine(text: String): IO[String] = IO(StdIn.readLine(text))
  }

  def main(args: Array[String]): Unit = {

    import Console._
    readLine("Enter your name: ").flatMap { name =>
      putStrLn(s" You name is $name")
    }.unsafeRunSync()

    //or using a for comprehension
    val program = for {
      name <- readLine("Enter your name: ")
      _ <- putStrLn(s" You name is $name")
    } yield ()

    program.unsafeRunSync()
  }

}
