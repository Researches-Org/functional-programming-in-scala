package chapter13

import scala.io.StdIn
import chapter11.Monad

trait IO[A] { self =>

  def run : A

  def map[B](f: A => B): IO[B] = new IO[B] {
    override def run = f(self.run)
  }

  def flatMap[B](f: A => IO[B]): IO[B] = new IO[B] {
    override def run = f(self.run).run
  }

  def ++(io: IO[A]): IO[A] = new IO[A] {
    override def run:A = {
      self.run
      io.run
    }
  }
}

object IO extends Monad[IO] {

  override def unit[A](a: => A):IO[A] = new IO[A] {
    override def run = a
  }

  override def flatMap[A, B](ma: IO[A])(f: (A) => IO[B]):IO[B] =
    ma flatMap f

  def apply[A](a: => A): IO[A] = unit(a)

  def PrintLine(msg: String): IO[Unit] = IO {
    println(msg)
  }

  def Readline: IO[String] = IO { StdIn.readLine }

}

case class Player(name: String, score: Int)

import IO._

// We could call these impure functions the “imperative shell” around the pure “core” of the program.
object Game {

  def contest(p1: Player, p2: Player): IO[Unit] =
    PrintLine(winnerMsg(winner(p1, p2)))

  def winnerMsg(p: Option[Player]): String = p map {
    case Player(name, _) => s"${name} is the winner!"
  } getOrElse("It is a draw!")

  def winner(p1: Player, p2: Player): Option[Player] =
    if (p1.score > p2.score)
      Some(p1)
    else if (p2.score > p1.score)
      Some(p2)
    else
      None

}

object Temperature {

  def fahrenheitToCelcius(f: Double): Double =
    (f - 32) * 5.0 / 9.0

  def converter: IO[Unit] = for {
    _ <- PrintLine("Enter a temperature in degrees fahrenheit: ")
    d <- Readline.map(_.toDouble)
    _ <- PrintLine(fahrenheitToCelcius(d).toString)
  } yield()

  def main(args: Array[String]): Unit = {
    val io = PrintLine("Enter a temperature in degrees fahrenheit:")
      .flatMap(_ => Readline.map(_.toDouble).map(d => PrintLine(fahrenheitToCelcius(d).toString).run))

    io.run
  }
}

