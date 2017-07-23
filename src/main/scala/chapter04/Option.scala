package chapter04

/**
  * It’s fine to use pattern matching, though you should be able to implement all
  * the functions besides map and getOrElse without resorting to pattern matching.
  *
  * For map and flatMap, the type signature should be enough to determine the implementation.
  *
  * getOrElse returns the result inside the Some case of the Option, or if the Option is None, returns the given
  * default value.
  *
  * orElse returns the first Option if it’s defined; otherwise, it returns the second Option.
  */
sealed trait Option[+A] {
  /**
    * Apply f if the Option is not None.
    */
  def map[B](f: A => B): Option[B] =
    this match {
      case None    => None
      case Some(a) => Some(f(a))
    }

  /**
    * Apply f, which may fail, to the Option if not None.
    */
  def flatMap[B](f: A => Option[B]): Option[B] =
    map(f) getOrElse None

  /**
    * The B >: A says that the B type parameter must be a supertype of A.
    * The => is actually syntactic sugar for a zero parameter function call:
    * x: () => Boolean
    * And as such x is not evaluated until the function is called so we get lazy evaluation of the parameter.
    * it's passing parameter by name. means expression will be evaluated when parameter is accessed.
    */
  def getOrElse[B >: A](default: => B): B =
    this match {
      case None    => default
      case Some(a) => a
    }

  /**
    * Don't evaluate ob unless needed.
    */
  def orElse[B >: A](ob: => Option[B]): Option[B] =
    map(Some(_)) getOrElse ob

  /**
    * Convert Some to None if the value doesn't satisfy f.
    */
  def filter(f: A => Boolean): Option[A] =
    flatMap(a => if (f(a)) Some(a) else None)

}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {

  /**
    * the map function lets us operate on values of type Option[A] using a
    * function of type A => B, returning Option[B]. Another way of looking at this is that map
    * turns a function f of type A => B into a function of type Option[A] => Option[B].
    *
    * This tells us that any function that we already have lying around can be transformed
    * (via lift) to operate within the context of a single Option value.
    */
  def lift[A,B](f: A => B): Option[A] => Option[B] = _ map f

  /**
    * We accept the A argument non-strictly, so we can catch any exceptions that
    * occur while evaluating a and convert them to None.
    */
  def Try[A](a: => A): Option[A] =
    try Some(a)
    catch {
      case e: Exception => None
    }

  /**
    * combines two Option values using a binary function. If either Option value is None, then the return value is too
    */
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    (a, b) match {
      case (None, _)          => None
      case (_, None)          => None
      case (Some(a), Some(b)) => Some(f(a, b))
    }

  def absO: Option[Double] => Option[Double] = lift(math.abs)

}

object OptionApp {

  def main(args: Array[String]): Unit = {
    println(None filter (_ => true))

    println(Some(1) filter (_ => true))

    println(Some(1) filter (_ => false))
  }

}