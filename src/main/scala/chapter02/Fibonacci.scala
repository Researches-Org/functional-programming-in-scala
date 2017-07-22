package chapter02

object Fibonacci {

  /**
    * The first two Fibonacci numbers are 0 and 1. The nth number is always the sum of the previous two—the sequence
    * begins 0, 1, 1, 2, 3, 5.
    * @param n
    * @return
    */
  def fib(n: Long): Long = {
    @annotation.tailrec
    def go(n: Long, previous: Long, current: Long): Long = {
      if (n == 0) previous
      else go(n - 1, current, previous + current)
    }

    go(n, 0, 1)
  }

  def main(args: Array[String]): Unit = {

    (0 to 90).foreach(n => {
      println(s"Fib(${n}) is: ${fib(n)}")
    })

  }

}