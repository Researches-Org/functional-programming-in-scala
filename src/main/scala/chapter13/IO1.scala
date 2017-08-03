package chapter13

object IO1 {


  sealed trait IO[A] {

    def flatMap[B](f: A => IO[B]): IO[B] =
      FlatMap(this, f)

    def map[B](f: A => B): IO[B] =
      flatMap(f andThen (Return(_)))

  }

  case class Return[A](a: A) extends IO[A]
  case class Suspend[A](resume: () => A) extends IO[A]
  case class FlatMap[A, B](sub: IO[A], k: A => IO[B]) extends IO[B]


  object IO {

    def printLine(s: String): IO[Unit] =
      Suspend(() => Return(println(s)))

  }

}


