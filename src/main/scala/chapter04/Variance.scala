package chapter04

object Variance {

  /**
    * The return type now reflects the possibility that the result may not always be defined.
    * We still always return a result of the declared type (now Option[Double]) from our
    * function, so mean is now a total function. It takes each value of the input type to exactly
    * one value of the output type.
    */
  def mean(xs: Seq[Double]): Option[Double] = {
    if (xs.isEmpty)
      None
    else
      Some(xs.sum / xs.length)
  }

  /**
    * Implement the variance function in terms of flatMap. If the mean of a sequence is m, the variance is the mean
    * of math.pow(x - m, 2) for each element x in the sequence.
    */
  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs) flatMap (m => mean(xs.map(x => math.pow(x - m, 2))))

}
