package chapter07

import java.util.concurrent._

/**
  * if unit begins evaluating its argument concurrently, then calling get arguably
  * breaks referential transparency. We can see this by replacing sumL and sumR with their
  * definitions—if we do so, we still get the same result, but our program is no longer
  * parallel:
  *
  * Par.get(Par.unit(sum(l))) + Par.get(Par.unit(sum(r)))
  *
  * If unit starts evaluating its argument right away, the next thing to happen is that get
  * will wait for that evaluation to complete. So the two sides of the + sign won’t run in
  * parallel if we simply inline the sumL and sumR variables. We can see that unit has a definite
  * side effect, but only with regard to get. That is, unit simply returns a Par[Int] in
  * this case, representing an asynchronous computation. But as soon as we pass that Par
  * to get, we explicitly wait for it, exposing the side effect. So it seems that we want to
  * avoid calling get, or at least delay calling it until the very end. We want to be able to
  * combine asynchronous computations without waiting for them to finish.
  */
object Par {

  type Par[A] = ExecutorService => Future[A]

  /**
    * Unit is represented as a function that returns a UnitFuture, which is a
    * simple implementation of Future that just wraps a constant value. It doesn't
    * use the ExecutorService at all. It's always done and can't be cancelled. Its
    * get method simply returns the value that we gave it.
    *
    * @param get - return the value of type A
    * @tparam A - type parameter of the value to be returned
    */
  private case class UnitFuture[A](get: A) extends Future[A] {

    override def isDone = true

    override def isCancelled = false

    override def get(timeout: Long, unit: TimeUnit) = get

    override def cancel(mayInterruptIfRunning: Boolean) = false

  }

  private case class Map2Future[A, B, C](a: Future[A], b: Future[B], f: (A,B) => C) extends Future[C] {

    @volatile var cache: Option[C] = None

    override def isDone = cache.isDefined

    override def isCancelled = a.isCancelled || b.isCancelled

    override def cancel(mayInterruptIfRunning: Boolean) =
      a.cancel(mayInterruptIfRunning) || b.cancel(mayInterruptIfRunning)

    override def get() = compute(Long.MaxValue)

    override def get(timeout: Long, unit: TimeUnit) =
      compute(TimeUnit.NANOSECONDS.convert(timeout, unit))

    private def compute(timeoutInNanos: Long): C =
      cache match {
        case Some(c) => c
        case None =>
          val start = System.nanoTime()
          val ar = a.get(timeoutInNanos, TimeUnit.NANOSECONDS)
          val stop = System.nanoTime()
          val aTime = stop - start
          val br = b.get(timeoutInNanos - aTime, TimeUnit.NANOSECONDS)
          val ret = f(ar, br)
          cache = Some(ret)
          ret
      }

  }

  /**
    * If fork begins evaluating its argument immediately in parallel, the implementation
    * must clearly know something, either directly or indirectly, about how to create
    * threads or submit tasks to some sort of thread pool. Moreover, this implies that the
    * thread pool (or whatever resource we use to implement the parallelism) must be
    * (globally) accessible and properly initialized wherever we want to call fork.4 This
    * means we lose the ability to control the parallelism strategy used for different parts of
    * our program. And though there’s nothing inherently wrong with having a global
    * resource for executing parallel tasks, we can imagine how it would be useful to have
    * more fine-grained control over what implementations are used where (we might like
    * for each subsystem of a large application to get its own thread pool with different
    * parameters, for example). It seems much more appropriate to give get the responsibility
    * of creating threads and submitting execution tasks.
    *
    * In contrast, if fork simply holds on to its unevaluated argument until later, it
    * requires no access to the mechanism for implementing parallelism. It just takes an
    * unevaluated Par and “marks” it for concurrent evaluation. Let’s now assume this
    * meaning for fork. With this model, Par itself doesn't need to know how to actually
    * implement the parallelism. It’s more a description of a parallel computation that gets
    * interpreted at a later time by something like the get function. This is a shift from
    * before, where we were considering Par to be a container of a value that we could simply
    * get when it becomes available. Now it’s more of a first-class program that we can run. So
    * let's rename our get function to run, and dictate that this is where the parallelism
    * actually gets implemented
    */

  /**
    * This is the simplest and most natural implementation of fork,but there are some
    * problems with it—for one, the outer Callable will block waiting for the “inner”
    * task to complete. Since this blocking occupies a thread in our thread pool, or
    * whatever resource backs the ExecutorService, this implies that we’re losing out
    * on some potential parallelism. Essentially, we’re using two threads when one should
    * suffice. This is a symptom of a more serious problem with the implementation that
    * we’ll discuss later in the chapter.
    * @param a
    * @tparam A
    * @return - Parallel description
    */
  def fork[A](a: => Par[A]): Par[A] =
    es => es.submit(new Callable[A] {
      def call = a(es).get
    })

  /**
    * Let's now return to the question of whether unit should be strict or lazy. With
    * fork, we can now make unit strict without any loss of expressiveness. A non-strict version
    * of it, lets call it lazyUnit, can be implemented using unit and fork
    */

  /**
    * unit promotes a constant value to a parallel computation.
    *
    * @param a - constant value
    * @tparam A - the type of the value
    * @return - Parallel computation description
    */
  def unit[A](a: A): Par[A] = (_: ExecutorService) => UnitFuture(a)

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

  /**
    * run extracts a value from a Par by actually performing the computation.
    *
    * @param a - Parallel computation description to be performed
    * @tparam A - Type of the extracted value of the parallel computation
    * @return - Extracted value of the parallel computation
    */
  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  /**
    * Par.map2 is a new higher-order function for combining the result of two parallel computations.
    * What is its signature? Give the most general signature possible (don't
    * assume it works only for Int).
    *
    * Discussion about map2 be non-strict (lazy) or strict:
    * if map2 is strict, and Scala evaluates arguments left to
    * right, whenever we encounter map2(sum(x),sum(y))(_ + _), we have to then evaluate
    * sum(x) and so on recursively. This has the rather unfortunate consequence that
    * we'll strictly construct the entire left half of the tree of summations first before moving
    * on to (strictly) constructing the right half. Here sum(IndexedSeq(1,2)) gets fully
    * expanded before we consider sum(IndexedSeq(3,4)). And if map2 evaluates its arguments
    * in parallel (using whatever resource is being used to implement the parallelism,
    * like a thread pool), that implies the left half of our computation will start
    * executing before we even begin constructing the right half of our computation.
    *
    * What if we keep map2 strict, but don’t have it begin execution immediately? Does
    * this help? If map2 doesn't begin evaluation immediately, this implies a Par value is
    * merely constructing a description of what needs to be computed in parallel. Nothing
    * actually occurs until we evaluate this description, perhaps using a get-like function.
    * The problem is that if we construct our descriptions strictly, they'll be rather heavyweight
    * objects. Looking back at our trace, our description will have to contain the full
    * tree of operations to be performed. Whatever data structure we use to store this description,
    * it’ll likely occupy more space than the original list itself! It would be nice if our descriptions
    * were more lightweight.
    *
    * we should make map2 lazy and have it begin immediate execution of both
    * sides in parallel. This also addresses the problem of giving neither side priority over
    * the other.
    */

  /**
    * map2 combines the results of two parallel computations with a binary function.
    *
    * map2 doesn't evaluate the call to f in a separate logical thread, in accord
    * with our design choice of having fork be the sole function in the API for
    * controlling parallelism. We can always do fork(map2(a,b)(f)) if we
    * want the evaluation of f to occur in a separate thread.
    *
    * This implementation of map2 usinf UnitFuture does not respect timeouts. It simply passes the
    * ExecutorService on to both Par values, waits for the results of the Futures af and
    * bf, applies f to them, and wraps them in a UnitFuture. In order to respect timeouts,
    * we’d need a new Future implementation that records the amount of time spent evaluating
    * af, and then subtracts that time from the available time allocated for evaluating bf.
    *
    * This new Future implementation is Map2Future
    *
    * @param a - Parallel computation description
    * @param b - other parallel computation description
    * @param f - binary function to combine the results of the two parallel computations
    * @tparam A - Type of the result of first parallel computation
    * @tparam B - Type of the result of second parallel computation
    * @tparam C - Type of the result of the binary function
    * @return - Parallel computation description that represents tha combination of both parallel computations
    */
  def map2[A, B, C](a: Par[A], b: Par[B])(f:(A, B) => C): Par[C] =
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      Map2Future(af, bf, f)
    }

  def map[A, B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(()))((a,_) => f(a))

  /**
    * Mark the list of integers to be sorted.
    *
    * @param parList - Par[List[Int that will be marked to have the list sorted.
    * @return - Par[List[Int marked to have the list sorted.
    */
  def sortPar(parList: Par[List[Int]]): Par[List[Int]] = map(parList)(_.sorted)

  /**
    * Hard: Write this function, called sequence. No additional primitives are required. Do not call run.
    * @param ps
    * @tparam A
    * @return
    */
  def sequence[A](ps: List[Par[A]]): Par[List[A]] =
    (es: ExecutorService) => UnitFuture( ps.map(p => p(es).get()) )

  def parMap[A,B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }

  /**
    * Implement parFilter, which filters elements of a list in parallel.
    * @param as - list to be filtered
    * @param f - filter function
    * @tparam A - type parameter of elements in the list
    * @return - Parallel computation description of list filtered
    */
  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] =
    unit(as.filter(f))

  /**
    * Is there a more general version of the parallel summation function we wrote at
    * the beginning of this chapter?
    * Try using it to find the maximum value of an IndexedSeq in parallel.
    *
    * Write a function that takes a list of paragraphs (a List[String]) and returns
    * the total number of words across all paragraphs, in parallel. Generalize this
    * function as much as possible.
    *
    * Implement map3, map4, and map5, in terms of map2.
    */
  def map3[A, B, C, D](a: Par[A], b: Par[B], c: Par[C])(f: A => B => C => D): Par[D] = {
    val pfd = map2(a, b)((a, b) => f(a)(b))
    map2(c, pfd)((c, fd) => fd(c))
  }

  def map4[A, B, C, D, E](a: Par[A], b: Par[B], c: Par[C], d: Par[D])(f: A => B => C => D => E): Par[E] = {
    val p1: Par[C => D => E] = map2(a, b)((a, b) => f(a)(b))
    val p2: Par[D => E] = map2(c, p1)((c, f) => f(c))
    map2(d, p2)((d, f) => f(d))
  }

  def map5[A, B, C, D, E, F](a: Par[A], b: Par[B], c: Par[C], d: Par[D], e: Par[E])
                            (f: A => B => C => D => E => F): Par[F] = {
    val p1 = map2(a, b)((a, b) => f(a)(b))
    val p2 = map2(c, p1)((c, f) => f(c))
    val p3 = map2(d, p2)((d, f) => f(d))
    map2(e, p3)((e, f) => f(e))
  }

  def sum(ints: IndexedSeq[Int]): Par[Int] =
    reduce(ints, 0)(_+_)

  def max(ints: IndexedSeq[Int]): Par[Int] =
    reduce(ints, Int.MinValue)(math.max)

  def reduce[A](values: IndexedSeq[A], default: A)(f: (A, A) => A): Par[A] =
    if (values.length <= 1)
      unit(values.headOption getOrElse default)
    else {
      val (l, r) = values.splitAt(values.length / 2)
      map2(fork(reduce(l, default)(f)), fork(reduce(r, default)(f)))(f)
    }

  def countWords(paragraphs: List[String]): Par[Int] =
    if (paragraphs.length == 0)
      unit(0)
    else if (paragraphs.length == 1)
      unit(paragraphs.head.split(" ").length)
    else {
      val (l, r) = paragraphs.splitAt(paragraphs.length / 2)
      map2(fork(countWords(l)), fork(countWords(r)))(_+_)
    }


}

import Par._

object ParApp {

  def main(args: Array[String]): Unit = {

    println(run(Executors.newCachedThreadPool())(countWords(List("1 2 3 1 2 3", "4 5 6 4 5 6", "1 2 3 4 5 6"))).get())

    println(run(Executors.newCachedThreadPool())(sum(IndexedSeq(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))).get())

  }

}
