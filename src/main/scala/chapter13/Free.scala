package chapter13

import chapter07.Par.Par

sealed trait Free[F[_],A] {

  def flatMap[B](f: A => Free[F,B]): Free[F,B] =
    FlatMapF(this, f)
  def map[B](f: A => B): Free[F,B] =
    flatMap(f andThen (ReturnF(_)))

}
case class ReturnF[F[_],A](a: A) extends Free[F,A]
case class SuspendF[F[_],A](s: F[A]) extends Free[F,A]
case class FlatMapF[F[_],A,B](s: Free[F,A], f: A => Free[F,B]) extends Free[F,B]

object Free {

  type TailRec[A] = Free[Function0,A]
  type Async[A] = Free[Par, A]

  @annotation.tailrec
  def runTrampoline[A](a: Free[Function0,A]): A = (a) match {
    case ReturnF(a) => a
    case SuspendF(r) => r()
    case FlatMapF(x,f:(Any=>Free[Function0,A])) => x match {
      case ReturnF(a) => runTrampoline { f(a) }
      case SuspendF(r) => runTrampoline { f(r()) }
      case FlatMapF(a0,g:(Any=>Free[Function0,Any])) => runTrampoline { a0 flatMap { a0 => g(a0) flatMap f } }
    }
  }

//  @annotation.tailrec
//  def runTrampoline[A](a: Free[Function0,A]): A = (a) match {
//    case ReturnF(a:A) => a
//    case SuspendF(r) => r()
//    case FlatMapF(x,f:(Any => Free[Function0,A])) => x match {
//      case ReturnF(a) => runTrampoline { f(a) }
//      case SuspendF(r) => runTrampoline { f(r()) }
//      case FlatMapF(a0,g:(Any=>Free[Function0,Any])) => runTrampoline { a0.flatMap { a0 => g(a0) flatMap f } }
//    }
//  }

}
