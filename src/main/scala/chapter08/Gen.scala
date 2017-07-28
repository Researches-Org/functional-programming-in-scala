package chapter08

import chapter06.{RNG, SimpleRNG, State}
import chapter08.Prop.{FailedCase, SuccessCount}
import Gen._

case class Gen[A](sample: State[RNG, A]) {

  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(sample.flatMap(a => f(a).sample))

  /* A method alias for the function we wrote earlier. */
  def listOfN(size: Int): Gen[List[A]] =
    Gen.listOfN(size, this)

  /* A version of `listOfN` that generates the size to use dynamically. */
  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size flatMap (n => this.listOfN(n))

}

object Gen {

  /**
    * Not having to explicitly specify sizes is powerful as well. It means that whatever
    * function runs the tests has the freedom to choose test case sizes, which opens up the
    * possibility of doing the test case minimization we mentioned earlier. If the sizes are
    * always fixed and specified by the programmer, the test runner won't have this flexibility.
    * Keep this concern in mind as we get further along in our design.
    */
  def listOf[A](a: Gen[A]): Gen[List[A]] = ???

//  def listOfN[A](n: Int, a: Gen[A]): Gen[List[A]] =
//    Gen(State(s => ((1 to n).map(_ => a.sample.run(s)._1).toList, s)))

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(g.sample)))

  def forAll[A](a: Gen[A])(f: A => Boolean): Prop = ???

  def unit[A](a: => A): Gen[A] = Gen(State.unit(a))

  def boolean: Gen[Boolean] =
    Gen(State(RNG.boolean))

  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive-start)))

}

object Prop {

  type FailedCase = String
  type SuccessCount = Int

}

trait Prop {

  def check: Either[(FailedCase, SuccessCount), SuccessCount]

  def &&(p: Prop): Prop = {
    val self = this
    new Prop {
      override def check = self.check match {
        case Left(v) => p.check match {
          case Left(v2) => Left(v._1 + v2._1, v._2 + v2._2 )
          case Right(v2) => Left(v._1, v._2 + v2)
        }
        case Right(v) => p.check match {
          case Left(v2) => Left(v2._1, v + v2._2 )
          case Right(v2) => Right(v + v2)
        }
      }
    }
  }

}

object GenApp {

  def main(args: Array[String]): Unit = {

    val rng = new SimpleRNG(197)

    val gen = choose(1, 10)

    (1 to 100).foreach(_ => println(gen.sample.run(rng)._1))

    val genBool = boolean

    (1 to 100).foreach( _ => println(genBool.sample.run(rng)._1))




  }

}

