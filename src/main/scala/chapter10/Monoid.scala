package chapter10

/**
  * A monoid consists of the following:
  *
  * Some type A
  *
  * An associative binary operation, op, that takes two values of type A and combines
  * them into one: op(op(x,y), z) == op(x, op(y,z)) for any choice of x: A, y: A, z: A
  *
  * A value, zero: A, that is an identity for that operation: op(x, zero) == x and
  * op(zero, x) == x for any x: A
  *
  * @tparam A
  */
trait Monoid[A] {

  /**
    * Satisfies op(op(x,y), z) == op(x, op(y,z))
    */
  def op(a1: A, a2: A) : A

  /**
    * Satisfies op(x, zero) == x and op(zero, x) == x
    */
  def zero: A

}

object Monoid {

  def foldMap[A,B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.map(f).foldLeft(m.zero)(m.op)

  // The function type `(A, B) => B`, when curried, is `A => (B => B)`.
  // And of course, `B => B` is a monoid for any `B` (via function composition).
  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
    foldMap(as, new EndoMonoid[B])(f.curried)(z)


  // Folding to the left is the same except we flip the arguments to
  // the function `f` to put the `B` on the correct side.
  // Then we have to also "flip" the monoid so that it operates from left to right.
  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
    foldMap(as, new DualMonoid[B => B](new EndoMonoid[B]()))(a => b => f(b, a))(z)

  def dual[A](m: Monoid[A]): Monoid[A] = new Monoid[A] {
    def op(x: A, y: A): A = m.op(y, x)
    val zero = m.zero
  }

  // There is a choice of implementation here as well.
  // Do we implement it as `f compose g` or `f andThen g`? We have to pick one.
  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    def op(f: A => A, g: A => A) = f compose g
    val zero = (a: A) => a
  }
}

class StringMonoid extends Monoid[String] {

  override def op(a1: String, a2: String): String = a1 + a2

  override def zero: String = ""

}

class ListMonoid[A] extends Monoid[List[A]] {

  override def op(a1: List[A], a2: List[A]): List[A] = a1 ++ a2

  override def zero: List[A] = Nil
}

class IntAdditionMonoid extends Monoid[Int] {

  override def op(a1: Int, a2: Int):Int = a1 + a2

  override def zero:Int = 0

}

class IntMultiplicationMonoid extends Monoid[Int] {

  override def op(a1: Int, a2: Int): Int = a1 * a2

  override def zero: Int = 1

}

class BooleanOrMonoid extends Monoid[Boolean] {

  override def op(a1: Boolean, a2: Boolean) : Boolean = a1 || a2

  override def zero: Boolean = false
}

class BooleanAndMonoid extends Monoid[Boolean] {

  override def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2

  override def zero: Boolean = true
}

class OptionMonoid[A] extends Monoid[Option[A]] {

  override def op(a1: Option[A], a2: Option[A]): Option[A] = a1.orElse(a2)

  override def zero: Option[A] = None

}

/**
  * A function having the same argument and return type is sometimes called an endofunction.
  * Write a monoid for endofunctions.
  */

class EndoMonoid[A] extends Monoid[A => A] {

  override def op(a1: (A) => A, a2: (A) => A): A => A = a => a1(a2(a))

  override def zero: A => A = a => a
}

// We can get the dual of any monoid just by flipping the `op`.
class DualMonoid[A](m: Monoid[A]) extends Monoid[A] {

  def op(x: A, y: A): A = m.op(y, x)
  val zero = m.zero
}
