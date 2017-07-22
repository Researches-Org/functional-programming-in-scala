package chapter02

object IsSorted {

  /**
    * checks whether an Array[A] is sorted according to a given comparison function:
    * @param as
    * @param ordered
    * @tparam A
    * @return
    */
  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {

    as.map(a => (a, true)).reduce((a, b) => {
      if (ordered(a._1, b._1)) (b._1, a._2)
      else (b._1, false)
    })._2

  }

  def main(args: Array[String]): Unit = {
    println(isSorted[Int](Array(1), _<=_))
    println(isSorted[Int](Array(1, 2), _<=_))
    println(isSorted[Int](Array(1, 2, 0), _<=_))
    println(isSorted[Int](Array(1, 2, 0, 1), _<=_))
  }

}
