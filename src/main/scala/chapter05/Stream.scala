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

  /**
    * Use unfold to implement map, take, takeWhile, zipWith (as in chapter 3), and
    * zipAll. The zipAll function should continue the traversal as long as either stream
    * has more elements—it uses Option to indicate whether each stream has been
    * exhausted.
    */

  def mapViaUnfold[B](f: A => B): Stream[B] =
    unfold(this) {
      case Empty => None
      case Cons(h, t) => Some( f(h()), t() )
    }

  def takeViaUnfold(n: Int): Stream[A] =
    unfold(this, n) (s => s._1 match {
      case Cons(h, t) if (s._2 > 0) => Some(h(), (t(), n - 1))
      case _                        => None

    })

  def takeWhileViaUnfold(p: A => Boolean): Stream[A] =
    unfold(this) {
      case Cons(h, t) if (p(h())) => Some(h(), t())
      case _                      => None
    }

  def zipWith[B, C](s2: Stream[B])(f: (A, B) => C): Stream[C] =
    unfold((this, s2)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some(f(h1(), h2()), (t1(), t2()))
      case _                            => None
    }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] =
    unfold((this, s2)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some((Some(h1()), Some(h2())), (t1(), t2()))
      case (Cons(h, t), _)              => Some((Some(h()), None), (t(), Empty))
      case (_, Cons(h, t))              => Some((None, Some(h())), (Empty, t()))
      case _                            => None
    }

  /**
    * INCORRECT DUE TO MISUSE OF NON-STRICTNESS:
    * This implementation of startsWithViaUnfold is incorrect because the function
    * unfold is called to produce a stream and this stream is never used, so the corecursion
    * in unfold is never executed and the local variable result is never updated. The reason
    * behind this the use of the non-strict (lazy) function cons whose second parameter is executed
    * only when necessary because of the laziness declaration on it.
    */
  def startsWithIncorrectViaUnfold[A](s: Stream[A]): Boolean = {
    var result = false

    unfold((this, s)) {
      case (Cons(h1, t1), Cons(h2, t2)) if (h1() == h2()) => Some(h1(), (t1(), t2()))
      case (_, Empty)                                     => {result = true; None}
      case (_, Cons(_, _))                                => None
    }

    result
  }

  def startsWithViaUnfold[A](s: Stream[A]): Boolean = {
    var result = false

    // use of toList to force the evaluation of corecursion in unfold due to the non-strict cons function
    unfold((this, s)) {
      case (Cons(h1, t1), Cons(h2, t2)) if (h1() == h2()) => Some(h1(), (t1(), t2()))
      case (_, Empty)                                     => {result = true; None}
      case (_, Cons(_, _))                                => None
    }.toList

    result
  }

  def startsWith[A](s: Stream[A]): Boolean =
    zipAll(s).takeWhile(!_._2.isEmpty) forAll {
      case (h,h2) => h == h2
    }

  /**
    * Implement tails using unfold. For a given Stream, tails returns the Stream of suffixes
    * of the input sequence, starting with the original Stream. For example, given
    * Stream(1,2,3), it would return Stream(Stream(1,2,3), Stream(2,3), Stream(3),
    * Stream()).
    */
  def tails: Stream[Stream[A]] =
    unfold(this) {
      case Cons(h, t) => Some( Cons(h, t) , t() )
      case Empty      => None
    } append Stream(empty)

  def hasSubsequence[A](s: Stream[A]): Boolean =
    tails exists (_ startsWith s)

  /**
    * Hard: Generalize tails to the function scanRight, which is like a foldRight that
    * returns a stream of the intermediate results. For example:
    * scala> Stream(1,2,3).scanRight(0)(_ + _).toList
    * res0: List[Int] = List(6,5,3,0)
    * This example should be equivalent to the expression List(1+2+3+0, 2+3+0, 3+0, 0).
    * Your function should reuse intermediate results so that traversing a Stream with n
    * elements always takes time linear in n. Can it be implemented using unfold? How, or
    * why not? Could it be implemented using another function we’ve written?
    */
  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
    foldRight((z, Stream(z)))((a, p0) => {
      // p0 is passed by-name and used in by-name args in f and cons. So use lazy val to ensure only one evaluation...
      lazy val p1 = p0
      val b2 = f(a, p1._1)
      (b2, cons(b2, p1._2))
    })._2


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

  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  //  Write a function fibs that generates the infinite stream of Fibonacci numbers: 0, 1, 1,
  //  2, 3, 5, 8, and so on.
  def fibs: Stream[Int] = {

    def loop(n1: Int, n2: Int): Stream[Int] = {
      cons(n1, loop(n2, n1 + n2))
    }

    loop(0, 1)
  }

  /**
    * Write a more general stream-building function called unfold. It takes an initial state,
    * and a function for producing both the next state and the next value in the generated
    * stream.
    *
    * Option is used to indicate when the Stream should be terminated, if at all. The function
    * unfold is a very general Stream-building function.
    * The unfold function is an example of what’s sometimes called a corecursive function.
    * Whereas a recursive function consumes data, a corecursive function produces
    * data. And whereas recursive functions terminate by recursing on smaller inputs, corecursive
    * functions need not terminate so long as they remain productive, which just
    * means that we can always evaluate more of the result in a finite amount of time. The
    * unfold function is productive as long as f terminates, since we just need to run the
    * function f one more time to generate the next element of the Stream. Corecursion is
    * also sometimes called guarded recursion, and productivity is also sometimes called
    * cotermination. These terms arent that important to our discussion, but you'll hear
    * them used sometimes in the context of functional programming. If you're curious to
    * learn where they come from and understand some of the deeper connections, follow
    * the references in the chapter notes.
    */
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case None         => empty
      case Some((a, s)) => cons(a, unfold(s)(f))
    }

  /**
    * Write fibs, from, constant, and ones in terms of unfold
    */

  def fibsViaUnfold: Stream[Int] =
    unfold((0, 1))(i => Some(i._1, (i._2, i._1 + i._2)))

  def fromViaUnfold(n: Int): Stream[Int] =
    unfold(n)(i => Some((i, i + 1)))

  def constantViaUnfold(n: Int): Stream[Int] =
    unfold(n)(i => Some(i, i))

  def onesViaUnfold = unfold(1)(_ => Some(1, 1))

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
      interleaving of map and filter operations:

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
    val stream = Stream(1, 2, 3, 4)

    println(stream.map(_ + 10).filter(_ % 2 == 0).toList)

    println(stream.map(_ + 10))

    println(stream.mapViaUnfold(_ + 10).toList)

    val stream2 = Stream(1,2)

    val stream3 = Stream(1, 2, 3, 4)

    val stream4 = Stream(1, 2, 3, 4, 5)

    println(stream.startsWith(stream2))
    println(stream.startsWithViaUnfold(stream2))

    println(stream.startsWith(stream3))
    println(stream.startsWithViaUnfold(stream3))

    println(stream.startsWith(stream4))
    println(stream.startsWithViaUnfold(stream4))

    println(stream.tails.toList.length)
    stream.tails.toList.foreach(s => {
      println(s.toList)
    })

  }

}