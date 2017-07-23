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

object OptionApp {

  def main(args: Array[String]): Unit = {
    println(None filter (_ => true))

    println(Some(1) filter (_ => true))

    println(Some(1) filter (_ => false))
  }

}