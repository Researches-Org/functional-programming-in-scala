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

  /**
    * Note that || is non-strict in its second argument. If p(h()) returns true, then exists
    * terminates the traversal early and returns true as well. Remember also that the tail of
    * the stream is a lazy val. So not only does the traversal terminate early, the tail of the
    * stream is never evaluated at all! So whatever code would have generated the tail is
    * never actually executed.
    */
  def exists(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) || t().exists(p)
    case _ => false
  }

  /**
    * If f doesn't evaluate its second argument, the recursion never occurs.
    * The arrow => in front of the argument type B means that the function f takes its second argument by
    * name and may choose not to evaluate it.
    */
  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

  def existsViaFoldRight(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  /**
    * Implement forAll, which checks that all elements in the Stream match a given predicate. Your implementation
    * should terminate the traversal as soon as it encounters a non-matching value.
    */
  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  /**
    * Use foldRight to implement takeWhile.
    */
  def takeWhileViaFoldRight(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, b) => if (p(a)) cons(a, b) else empty)

  /**
    * Hard: Implement headOption using foldRight.
    */
  def headOptionViaFoldRight: Option[A] =
    foldRight(None: Option[A])((h, _) => Some(h))

  /**
    * Implement map, filter, append, and flatMap using foldRight. The append method should be non-strict in its
    * argument.
    */
  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((a, b) => cons(f(a), b))

  def filter(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, b) => if (p(a)) cons(a, b) else b)

  def append[B >: A](s: => Stream[B]): Stream[B] =
    foldRight(s)((a, b) => cons(a, b))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((a, b) => f(a) append  b)

  def find(p: A => Boolean): Option[A] = filter(p).headOption
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

    /**
      The thing to notice in this trace is how the filter and map transformations are interleaved—
      the computation alternates between generating a single element of the output
      of map, and testing with filter to see if that element is divisible by 2 (adding it to
      the output list if it is). Note that we don’t fully instantiate the intermediate stream that
      results from the map. It’s exactly as if we had interleaved the logic using a special purpose
      loop. For this reason, people sometimes describe streams as “first-class loops”
      whose logic can be combined using higher-order functions like map and filter.
      Since intermediate streams aren't instantiated, it’s easy to reuse existing combinators
      in novel ways without having to worry that we’re doing more processing of the
      stream than necessary. For example, we can reuse filter to define find, a method to
      return just the first element that matches if it exists. Even though filter transforms
      the whole stream, that transformation is done lazily, so find terminates as soon as a
      match is found.

      The reason behind that is the use of non-strict function foldRight, if the function f (the second parameter of
      foldRight) doesn't evaluate its second argument, the recursion never occurs. After foldRight is called through
      map method the result stream is returned but it is not already fully processed, so the filter method is called
      and the filter is applied on the first element, when the second element has to be processed by filter, only at
      this moment the stream is processed and the map operation continues to the second element which is passed to
      filter and the interleaving of map and filter occurs.

      We can show this effect if foldRight is changed to receive the name of the operation that has called it and this
      name is print when function f is called inside its first parameter

      def foldRight[B](op: String)(z: => B)(f: (A, => B) => B): B =
        this match {
          case Cons(h,t) => f({println(op);  h()}, t().foldRight(op)(z)(f))
          case _ => z
        }

      def map[B](f: A => B): Stream[B] =
        foldRight("map")(empty[B])((a, b) => cons(f(a), b))

      def filter(p: A => Boolean): Stream[A] =
        foldRight("filter")(empty[A])((a, b) => if (p(a)) cons(a, b) else b)

      The call to Stream(1,2,3,4).map(_ + 10).filter(_ % 2 == 0).toList will produce the following result showing the
       interleave of map and filter operations:

      map
      filter
      map
      filter
      map
      filter
      map
      filter

      Another evidence of the lazy nature of non-strict functions is when the result of a map operation is print:

      println(Stream(1, 2, 3, 4).map(_ + 10))

      The result is something like this showing that the stream is not processed yet:

      Cons(chapter05.Stream$$$Lambda$7/897913732@58ceff1,chapter05.Stream$$$Lambda$8/1688019098@7c30a502)

     */
    val stream = Stream(1,2,3,4).map(_ + 10).filter(_ % 2 == 0).toList

    println(stream)

    println(Stream(1, 2, 3, 4).map(_ + 10))

  }

}