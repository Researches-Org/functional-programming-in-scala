package chapter07

trait Par[+A] {

}

case class ForkedPar[+A](a: Par[A]) extends Par[A]
case class UnitPar[+A](a: A) extends Par[A]

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
    * meaning for fork. With this model, Par itself doesn' need to know how to actually
    * implement the parallelism. It’s more a description of a parallel computation that gets
    * interpreted at a later time by something like the get function. This is a shift from
    * before, where we were considering Par to be a container of a value that we could simply
    * get when it becomes available. Now it’s more of a first-class program that we can run. So
    * let's rename our get function to run, and dictate that this is where the parallelism
    * actually gets implemented
    */

  def fork[A](a: => Par[A]): Par[A] = ForkedPar(a)

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
  def unit[A](a: A): Par[A] = UnitPar(a)

  /**
    * lazyUnit wraps its unevaluated argument in a Par and marks it for concurrent
    * evaluation.
    * @param a - unevaluated argument
    * @tparam A - type of the unevaluated argument
    * @return - Parallel computation description marked to be executed concurrently
    */
  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  /**
    * run extracts a value from a Par by actually performing the computation.
    *
    * @param a - Parallel computation description to be performed
    * @tparam A - Type of the extracted value of the parallel computation
    * @return - Extracted value of the parallel computation
    */
  def run[A](a: Par[A])(implicit te: ThreadExecutor): A = te.run(a)

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
    * @param a - Parallel computation description
    * @param b - other parallel computation description
    * @param f - binary function to combine the results of the two parallel computations
    * @tparam A - Type of the result of first parallel computation
    * @tparam B - Type of the result of second parallel computation
    * @tparam C - Type of the result of the binary function
    * @return - Parallel computation description that represents tha combination of both parallel computations
    */
  def map2[A, B, C](a: Par[A], b: Par[B])(f:(A, B) => C): Par[C] = UnitPar(f(run(a), run(b)))

}

trait ThreadExecutor {
  def run[A](a: Par[A]): A
}