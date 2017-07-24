package chapter05

import Stream._

object InfiniteStreamsApp {

  def main(args: Array[String]): Unit = {

    lazy val ones: Stream[Int] = Stream.cons(1, ones)

    // Although ones is infinite, the functions we’ve written so far only inspect the portion of
    // the stream needed to generate the demanded output
    println(ones.take(5).toList)

    println(ones.exists(_ % 2 != 0))

    println(ones.map(_ + 1).exists(_ % 2 == 0))

    println(ones.takeWhile(_ == 1))

    println(ones.forAll(_ != 1))

    val twos = constant(2)

    println(twos)

    println(twos.take(5).toList)

    println(from(0).take(11).toList)

    println(fibs.take(20).toList)

    println(fibsViaUnfold.take(20).toList)

    println(fromViaUnfold(0).take(11).toList)

    println(constantViaUnfold(2).take(5).toList)

    println(onesViaUnfold.take(5).toList)
  }

}

