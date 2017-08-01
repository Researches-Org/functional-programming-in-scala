package chapter11

import chapter07.Par.{Par, toParOps}
import fpinscala.testing.Gen

trait Monad[F[_]] extends Functor[F] {

  def unit[A](a: => A): F[A]

  def flatMap[A, B](ma: F[A])(f: A => F[B]): F[B]

  def map[A, B](a: F[A])(f: A => B): F[B] =
    flatMap(a)(a => unit(f(a)))

  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    flatMap(fa)(a => map(fb)(b => f(a, b)))

  def sequence[A](lma: List[F[A]]): F[List[A]] =
    lma.foldRight(unit(List[A]()))((ma, mla) => map2(ma, mla)(_ :: _))

  def traverse[A,B](la: List[A])(f: A => F[B]): F[List[B]] =
    la.foldRight(unit(List[B]()))((a, mlb) => map2(f(a), mlb)(_ :: _))
}

object Monad {

  val genMonad = new Monad[Gen] {

    override def unit[A](a: => A): Gen[A] = Gen.unit(a)

    override def flatMap[A, B](ma: Gen[A])(f: (A) => Gen[B]): Gen[B] =
      ma flatMap f

  }

  val parMonad = new Monad[Par] {
    override def unit[A](a: => A) = chapter07.Par.unit(a)

    override def flatMap[A, B](ma: Par[A])(f: (A) => Par[B]) =
      ma flatMap f
  }

  val optionMonad = new Monad[Option] {

    override def unit[A](a: => A) = Some(a)

    override def flatMap[A, B](ma: Option[A])(f: (A) => Option[B]) =
      ma flatMap f

  }

  val streamMonad = new Monad[Stream] {

    override def unit[A](a: => A) = Stream(a)

    override def flatMap[A, B](ma: Stream[A])(f: (A) => Stream[B]) =
      ma flatMap f

  }

  val listMonad = new Monad[List] {
    override def unit[A](a: => A) = List(a)

    override def flatMap[A, B](ma: List[A])(f: (A) => List[B]) =
      ma flatMap f
  }

}