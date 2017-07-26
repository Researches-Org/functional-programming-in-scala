package chapter06

sealed trait Input
case object Coin extends Input
case object Turn extends Input

/**
  * The rules of the machine are as follows:
  *
  * Inserting a coin into a locked machine will cause it to unlock if there’s any candy left.
  *
  * Turning the knob on an unlocked machine will cause it to dispense candy and become locked.
  *
  * Turning the knob on a locked machine or inserting a coin into an unlocked machine does nothing.
  *
  * A machine that’s out of candy ignores all inputs.
  *
  */
case class Machine(locked: Boolean, candies: Int, coins: Int) {

  def coin: Machine = if (locked && candies > 0) Machine(false, candies, coins + 1) else this

  def turn: Machine = if (!locked && candies > 0) Machine(true, candies - 1, coins) else this

  def run(i: Input): Machine =
    i match {
      case Coin => coin
      case Turn => turn
    }

  /**
    * The method simulateMachine should operate the machine based on the list of inputs
    * and return the number of coins and candies left in the machine at the end. For example,
    * if the input Machine has 10 coins and 5 candies, and a total of 4 candies are successfully
    * bought, the output should be (14, 1).
    */
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {

    var m = this

    inputs.foreach(i => {
      m = m.run(i)
    })

    State[Machine, (Int, Int)](_ => ( (m.coins, m.candies), m ))
  }
}

import State._
import Candy._

object Candy {
  def update =
    (i: Input) => (s: Machine) =>
      (i, s) match {
        case (_, Machine(_, 0, _)) => s
        case (Coin, Machine(false, _, _)) => s
        case (Turn, Machine(true, _, _)) => s
        case (Coin, Machine(true, candy, coin)) =>
          Machine(false, candy, coin + 1)
        case (Turn, Machine(false, candy, coin)) =>
          Machine(true, candy - 1, coin)
      }


  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = for {
    _ <- sequence(inputs map (modify[Machine] _ compose update))
    s <- get
  } yield (s.coins, s.candies)

  /**
    * Implementation using flatMap and map.
    *
    * At first each input is transformed into a State[Machine, Unit] through the call:
    *
    * inputs.map(i => modify[Machine](update(i)))
    *
    * So we will have a list of State[Machine, Unit]
    *
    * The sequence method transform this list into a State[Machine, List[Unit]].
    * The execution of each state is performed so the final Machine will have the
    * result.
    *
    * With the final state State[Machine, List[Unit]] the map is executed to
    * transform the result value in a pair of coins and candies.
    */
  def simulateMachine1(inputs: List[Input]): State[Machine, (Int, Int)] =
    sequence(inputs.map(i => modify[Machine](update(i)))).flatMap(_ => get.map(s => (s.coins, s.candies)))
}

object MachineApp {
  def main(args: Array[String]): Unit = {
    val inputs = List(Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn)

    val v1 = simulateMachine1(inputs)

    println(v1.run(Machine(true, 5, 10))._1)

  }
}
