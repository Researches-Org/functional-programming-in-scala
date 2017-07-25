package chapter06

/**
  * Simple random number generator that uses the same algorithm as scala.util.Random,
  * which happens to be what's called a linear congruential generator (http://mng.bz/r046).
  *
  * @param seed - Long that represents the seed to generate pseudo-random number.
  */
case class SimpleRNG(seed: Long) extends RNG {

  override def nextInt:(Int, RNG) = {
    // & is bitwise AND. We use the current seed to generate a new seed.
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL

    // The next  state, which is an RNG instance created from the new seed.
    val nextRGN = new SimpleRNG(newSeed)

    // >>> is right binary shift with zero fill. The value n is the new pseudo-random integer
    val n = (newSeed >>> 16).toInt

    (n, nextRGN)
  }


}

object SimpleRNGApp {

  def main(args: Array[String]): Unit = {
    // We can run this sequence of statements as many times as we want and we'll always get
    // the same values. When we call rng.nextInt, it will always return 16159453 and a new
    // RNG, whose nextInt will always return -1281479697. In other words, this API is pure.

    // This problem of making seemingly stateful APIs pure and its solution (having the API
    // compute the next state rather than actually mutate anything) aren't unique to random
    //  number generation. It comes up frequently, and we can always deal with it in this same
    //  way.

    val rng = new SimpleRNG(42)

    val (n1, rng2) = rng.nextInt

    println(s"pseudo-random: ${n1}")
    println(s"Next RNG: ${rng2}")

    val (n2, rng3) = rng2.nextInt

    println(s"pseudo-random: ${n2}")
    println(s"Next RNG: ${rng3}")

    println(Int.MinValue)
    println(Int.MaxValue)


  }

}
