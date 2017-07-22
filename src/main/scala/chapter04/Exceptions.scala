package chapter04

object Exceptions {

  /**
    * We can prove that y is not referentially transparent. Recall that any RT expression may
    * be substituted with the value it refers to, and this substitution should preserve program
    * meaning. If we substitute throw new Exception("fail!") for y in x + y, it produces
    * a different result, because the exception will now be raised inside a try block
    * that will catch the exception and return 43.
    */
  def failingFn(i: Int): Int = {
    val y: Int = throw new Exception("fail!")

    try {
      val x = 42 + 5
      x + y
    } catch {
      case e: Exception => 43
    }
  }

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5

      // A thrown Exception can be given any type; here we’re annotating it with the type Int.
      x + ((throw new Exception("fail!")):Int)
    } catch {
      case _: Exception => 43
    }
  }

  /**
    * Seq is the common interface of various linear sequence-like collections.
    * The mean function is an example of what’s called a partial function: it’s not defined for some inputs.
    */
  def mean(xs: Seq[Double]): Double = {

    if (xs.isEmpty)
      throw new ArithmeticException("mean of empty list!")

    xs.sum / xs.length
  }

  /**
    * This makes mean into a total function, but it has drawbacks—it requires that immediate
    * callers have direct knowledge of how to handle the undefined case and limits them to
    * returning a Double. What if mean is called as part of a larger computation and we’d
    * like to abort that computation if mean is undefined? Or perhaps we’d like to take some
    * completely different branch in the larger computation in this case? Simply passing an
    * onEmpty parameter doesn’t give us this freedom.
    * We need a way to defer the decision of how to handle undefined cases so that they
    * can be dealt with at the most appropriate level.
    */
  def mean_1(xs: IndexedSeq[Double], onEmpty: Double): Double = {
    if (xs.isEmpty)
      onEmpty
    else
      xs.sum / xs.length
  }

  /**
    * The return type now reflects the possibility that the result may not always be defined.
    * We still always return a result of the declared type (now Option[Double]) from our
    * function, so mean is now a total function. It takes each value of the input type to exactly
    * one value of the output type.
    */
  def mean_2(xs: Seq[Double]): Option[Double] = {
    if (xs.isEmpty)
      None
    else
      Some(xs.sum / xs.length)
  }

  def main(args: Array[String]): Unit = {
    println(failingFn2(12))

    val l = Seq[Double]()

    val d = l.sum / l.length

    println(d)

    println(d + 1)

  }

}
