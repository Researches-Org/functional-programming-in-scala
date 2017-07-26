package chapter07

import ListAddition._

object ListAddition {

  def sum(ints: Seq[Int]): Int =
    ints.foldLeft(0)(_+_)

  def sum(ints: IndexedSeq[Int]): Int =
    if (ints.length <= 1)
      ints.headOption getOrElse 0
    else {
      val (l, r) = ints.splitAt(ints.length / 2)
      sum(l) + sum(r)
    }

}

object ListAdditionApp {

  def main(args: Array[String]): UnitPar = {
    println(sum( List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10) ))

    println(sum( Array(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)))
  }

}
