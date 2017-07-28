package chapter07

import java.util.concurrent.{Callable, CountDownLatch, ExecutorService, Executors}
import java.util.concurrent.atomic.AtomicReference

import Nonblocking.Par._

/**
  * Using local side effects for a pure API
  * The Future type we defined here is rather imperative. An A => Unit? Such a function
  * can only be useful for executing some side effect using the given A, as we certainly
  * aren't using the returned result. Are we still doing functional programming in using a
  * type like Future? Yes, but we're making use of a common technique of using side
  * effects as an implementation detail for a purely functional API. We can get away with
  * this because the side effects we use are not observable to code that uses Par. Note
  * that Future.apply is protected and can't even be called by outside code.
  * As we go through the rest of our implementation of the non-blocking Par, you may
  * want to convince yourself that the side effects employed can't be observed by outside
  * code. The notion of local effects, observability, and subtleties of our definitions of
  * purity and referential transparency are discussed in much more detail in chapter 14,
  * but for now an informal understanding is fine.
  */
object Nonblocking {

  /**
    * Future with which we can register a callback that will be invoked when the result is ready.
    * @tparam A the type of callback parameter
    */
  sealed trait Future[+A] {

    /**
      * The apply method is declared private to the chapter07 package, which means
      * that it can only be accessed by code within that package. This is so that our API
      * remains pure and we can guarantee that our laws hold.
      *
      * @param k - callback that will be invoked when result is ready.
      */
    private[chapter07] def apply(k: A => Unit): Unit

  }

  type Par[+A] = ExecutorService => Future[A]

  object Par {

    /**
      * We could even go so far as to remove run from our API altogether and expose the apply method
      * on Par instead so that users can register asynchronous callbacks. That would certainly be a valid
      * design choice, but we’ll leave our API as it is for now.
      *
      * @param es - ExecutorService
      * @param p - Parallel description of a computation
      * @tparam A - type parameter of the result
      * @return - result of the parallel computation.
      */
    def run[A](es: ExecutorService)(p: Par[A]): A = {

      //A mutable, thread-safe reference to use for storing the result. See the java.util.concurrent.atomic
      // package for more information about these classes.
      val ref = new AtomicReference[A]

      // A java.util.concurrent.CountDownLatch allows threads to wait until its countDown method
      // is called a certain number of times. Here the countDown method will be called once when we've
      // received the value of type A from p, and we want the run implementation to block until that happens.
      val latch = new CountDownLatch(1)

      // When we receive the value through the Future, sets the result in the registered callback and
      // releases the latch.
      p(es) { a => ref.set(a); latch.countDown() }

      // Waits until the result becomes available and the latch is released.
      latch.await

      // Once we've passed the latch, we know ref has been set, and we return its value.
      ref.get
    }

    /**
      * Simply passes the value to the continuation (callback). Note that the ExecutorService isn't needed
      *
      * The same as:
      *
      * {{{
      *
      *   def unit[A](a: A): Par[A] =
      *     es => new Future[A] {
      *       def apply(cb: A => Unit): Unit = cb(a)
      *     }
      *
      * }}}
      *
      * Since unit already has a value of type A available, all it needs to do is call the continuation
      * cb, passing it this value. If that continuation is the one from our run implementation, for example,
      * this will release the latch and make the result available immediately.
      *
      * @param a - Parallel computation will return this value
      * @tparam A - type of the returned value
      * @return - Parallel computation that returns value a.
      */
    def unit[A](a: A): Par[A] =
      es => new Future[A] {
        def apply(cb: A => Unit): Unit =
          cb(a)
      }

    /**
      * Eval forks off evaluation of a and returns immediately. The callback will be invoked
      * asynchronously on another thread.
      *
      * @param a - Parallel computation
      * @tparam A - Type of returned value of parallel computation
      * @return - Parallel computation that will be forked off.
      */
    def fork[A](a: => Par[A]): Par[A] =
      es => new Future[A] {
        def apply(cb: A => Unit): Unit =
          eval(es)(a(es)(cb))
      }

    /**
      * helper function to evaluate an action asynchronously using some ExecutorService.
      *
      * @param es - ExecutorService
      * @param r - action to be evaluated
      * @tparam A - type parameter that represents the type of the result of the action.
      */
    def eval[A](es: ExecutorService)(r: => Unit): Unit =
      es.submit(new Callable[Unit] {def call = r})

    /**
      * Here, a non-blocking implementation is considerably trickier. Conceptually, we'd like
      * map2 to run both Par arguments in parallel. When both results have arrived, we want
      * to invoke f and then pass the resulting C to the continuation. But there are several
      * race conditions to worry about here, and a correct non-blocking implementation is
      * difficult using only the low-level primitives of java.util.concurrent.
      *
      * A BRIEF INTRODUCTION TO ACTORS
      * To implement map2, we'll use a non-blocking concurrency primitive called actors. An
      * Actor is essentially a concurrent process that doesn't constantly occupy a thread.
      * Instead, it only occupies a thread when it receives a message. Importantly, although
      * multiple threads may be concurrently sending messages to an actor, the actor processes
      * only one message at a time, queueing other messages for subsequent processing.
      * This makes them useful as a concurrency primitive when writing tricky code that
      * must be accessed by multiple threads, and which would otherwise be prone to race
      * conditions or deadlocks.
      *
      * @param p1
      * @param p2
      * @param f
      * @tparam A
      * @tparam B
      * @tparam C
      * @return
      */
    def map2[A, B, C](p1: Par[A], p2: Par[B])(f: (A, B) => C): Par[C] =
      es => new Future[C] {

        def apply(cb: C => Unit): Unit = {
          // Two mutable vars are used to store the two results.
          var ar: Option[A] = None
          var br: Option[B] = None

          // An actor that awaits both results, combines them with f, and passes the result to cb.
          val combiner = Actor[Either[A, B]](es) {

            // If the A result came in first, stores it in ar and waits for the B. If the A result came
            // last and we already have our B, calls f with both results and passes the resulting
            // C to the callback, cb.
            case Left(a) => br match {
              case None => ar = Some(a)
              case Some(b) => eval(es)(cb(f(a, b)))
            }

            // Analogously, if the B result came in first, stores it in br and waits for the A. If the B
            // result came last and we already have our A, calls f with both results and passes the
            // resulting C to the callback, cb.
            case Right(b) => ar match {
              case None => br = Some(b)
              case Some(a) => eval(es)(cb(f(a, b)))
            }
          }

          // Passes the actor as a continuation to both sides. On the A side, we wrap the result in
          // Left, and on the B side, we wrap it in Right. These are the constructors of the Either
          // data type, and they serve to indicate to the actor where the result came from.
          p1(es)(a => combiner ! Left(a))
          p2(es)(b => combiner ! Right(b))
        }
      }

    /**
      * lazyUnit wraps its unevaluated argument in a Par and marks it for concurrent
      * evaluation.
      * @param a - unevaluated argument
      * @tparam A - type of the unevaluated argument
      * @return - Parallel computation description marked to be executed concurrently
      */
    def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

    /**
      * convert any function A => B to one that evaluates its result asynchronously.
      *
      * @param f - Function to be evaluated asynchronously
      * @tparam A - Type parameter that represents the type of input parameter of f
      * @tparam B - Type parameter that represents the type of output of f
      * @return - converted function of type A => Par[B]
      */
    def asyncF[A,B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

    def sequenceRight[A](as: List[Par[A]]): Par[List[A]] =
      as match {
        case Nil => unit(Nil)
        case h :: t => map2(h, fork(sequence(t)))(_ :: _)
      }

    def sequenceBalanced[A](as: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] = fork {
      if (as.isEmpty) unit(Vector())
      else if (as.length == 1) map(as.head)(a => Vector(a))
      else {
        val (l,r) = as.splitAt(as.length/2)
        map2(sequenceBalanced(l), sequenceBalanced(r))(_ ++ _)
      }
    }

    // specialized version of `map`
    def map[A,B](p: Par[A])(f: A => B): Par[B] =
      es => new Future[B] {
        def apply(cb: B => Unit): Unit =
          p(es)(a => eval(es) { cb(f(a)) })
      }

    def sequence[A](as: List[Par[A]]): Par[List[A]] =
      map(sequenceBalanced(as.toIndexedSeq))(_.toList)

    def parMap[A,B](as: List[A])(f: A => B): Par[List[B]] =
      sequence(as.map(asyncF(f)))

    def choice[A](p: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
      es => new Future[A] {
        def apply(cb: A => Unit): Unit =
          p(es) { b =>
            if (b) eval(es) { t(es)(cb) }
            else eval(es) { f(es)(cb) }
          }
      }

    def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
      es => new Future[A] {
        override def apply(cb: (A) => Unit): Unit = {
          n(es) {
            i => eval(es){ choices(i)(es)(cb) }
          }
        }
      }

    def choiceMap[K,V](key: Par[K])(choices: Map[K,Par[V]]): Par[V] =
      es => new Future[V] {
        override def apply(cb: (V) => Unit): Unit = {
          key(es) {
            k => eval(es) { choices(k)(es)(cb) }
          }
        }
      }

    def choiceViaChoiceN[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
      choiceN(map(cond)(b => if (b) 0 else 1))(List(t, f))

    def chooser[A,B](pa: Par[A])(choices: A => Par[B]): Par[B] =
      es => new Future[B] {
        override def apply(cb: (B) => Unit): Unit = {
          pa(es) {
            a => choices(a)(es)(cb)
          }
        }
      }

    def flatMap[A,B](p: Par[A])(f: A => Par[B]): Par[B] =
      es => new Future[B] {
        def apply(cb: B => Unit): Unit =
          p(es)(a => f(a)(es)(cb))
      }

    def choiceViaFlatMap[A](p: Par[Boolean])(f: Par[A], t: Par[A]): Par[A] =
      flatMap(p)(b => if (b) t else f)

    def choiceNViaFlatMap[A](p: Par[Int])(choices: List[Par[A]]): Par[A] =
      flatMap(p)(i => choices(i))

    def join[A](p: Par[Par[A]]): Par[A] =
      es => new Future[A] {
        def apply(cb: A => Unit): Unit =
          p(es)(p2 => eval(es) { p2(es)(cb) })
      }

    def joinViaFlatMap[A](a: Par[Par[A]]): Par[A] =
      flatMap(a)(x => x)

    def flatMapViaJoin[A,B](p: Par[A])(f: A => Par[B]): Par[B] =
      join(map(p)(f))

    /* Gives us infix syntax for `Par`. */
    implicit def toParOps[A](p: Par[A]): ParOps[A] = new ParOps(p)

    // infix versions of `map`, `map2` and `flatMap`
    class ParOps[A](p: Par[A]) {
      def map[B](f: A => B): Par[B] = Par.map(p)(f)
      def map2[B,C](b: Par[B])(f: (A,B) => C): Par[C] = Par.map2(p,b)(f)
      def flatMap[B](f: A => Par[B]): Par[B] = Par.flatMap(p)(f)
      def zip[B](b: Par[B]): Par[(A,B)] = p.map2(b)((_,_))
    }

  }

}

object NonblockingApp {

  def main(args: Array[String]): Unit = {

    val p = parMap(List.range(1, 10))(math.sqrt(_))

    val x = run(Executors.newFixedThreadPool(2))(p)

    println(x)

    System.exit(0)

  }

}