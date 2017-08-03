package chapter13

import chapter07.Par
import chapter07.Par.Par

sealed trait Async[A] {

  def flatMap[B](f: A => Async[B]): Async[B] =
    FlatMapA(this, f)

  def map[B](f: A => B): Async[B] =
    flatMap(f andThen (ReturnA(_)))

}
case class ReturnA[A](a: A) extends Async[A]
case class SuspendA[A](resume: Par[A]) extends Async[A]
case class FlatMapA[A,B](sub: Async[A], k: A => Async[B]) extends Async[B]

object Async {

  //@annotation.tailrec Error:(23, 89) could not optimize @tailrec annotated method step: it contains a recursive call not in tail position
  //case FlatMapA(FlatMapA(x:Async[A],f:(Any => Async[A])), g:(Any => Async[A])) => step(null)
  def step[A](async: Async[A]): Async[A] = async match {
    case FlatMapA(FlatMapA(x,f:(Any => Async[A])), g:(Any => Async[A])) => step(x flatMap (a => f(a) flatMap g))
    case FlatMapA(ReturnA(x), f:(Any => Async[A])) => step(f(x))
    case _ => async
  }

//  def run[A](async: Async[A]): Par[A] = step(async) match {
//    case ReturnA(a) => Par.unit(a)
//    case SuspendA(r) => Par.flatMap(r)(a => run(a))
//    case FlatMapA(x, f) => x match {
//      case SuspendA(r) => Par.flatMap(r)(a => run(f(a)))
//      case _ => sys.error("Impossible; `step` eliminates these cases")
//    }
//  }


//  @annotation.tailrec
//  def runTrampoline[A](a: Free[Function0,A]): A = (a) match {
//    case ReturnF(a) => a
//    case SuspendF(r) => r()
//    case FlatMapF(x,f:(Any=>Free[Function0,A])) => x match {
//      case ReturnF(a) => runTrampoline { f(a) }
//      case SuspendF(r) => runTrampoline { f(r()) }
//      case FlatMapF(a0,g:(Any=>Free[Function0,Any])) => runTrampoline { a0 flatMap { a0 => g(a0) flatMap f } }
//    }
//  }

}



