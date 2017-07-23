package chapter05

import Stream._

sealed trait Stream[+A] {

  def headOption: Option[A] =
    this match {
      case Empty      => None
      case Cons(h, _) => Some(h())
    }

  def toList: List[A] =
    this match {
      case Empty => Nil
      case Cons(h, t) => h() :: t().toList
    }

  def take(n: Int): Stream[A] =
    this match {
      case Cons(h, t) if (n > 1)  => cons(h(), t().take(n - 1))
      case Cons(h, _) if (n == 1) => cons(h(), empty)
      case _                      => empty
    }

  @annotation.tailrec
  final def drop(n: Int): Stream[A] =
    this match {
      case Cons(_, t) if (n > 0) => t().drop(n - 1)
      case _                     => this
    }

  def takeWhile(p: A => Boolean): Stream[A] =
    this match {
      case Cons(h, t) if (p(h())) => cons(h(), t().takeWhile(p))
      case _                      => empty
    }

}
case object Empty extends Stream[Nothing]

/**
  * A nonempty stream consists of a head and a tail, which are both non-strict. Due to technical
  * limitations, these are thunks that must be explicitly forced, rather than by-name parameters
  */
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {

  /**
    * A smart constructor for creating a nonempty stream.
    */
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    // We cache the head and tail as lazy values to avoid repeated evaluation
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  /**
    * A smart constructor for creating an empty stream of a particular type
    */
  def empty[A]: Stream[A] = Empty

  /**
    * A convenient variable-argument method for constructing a Stream from multiple elements.
    */
  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

}

object StreamApp {

  def main(args: Array[String]): Unit = {

    val stream = cons(1, cons(2, cons(3, empty)))

    println(stream)

    println(stream.toList)

  }

}