package chapter14

object QuickSort {

  def quickSort(xs: List[Int]): List[Int] =
    if (xs.isEmpty)
      xs
    else {
      val arr = xs.toArray
      def swap(i: Int, j: Int): Unit = {
        val tmp = arr(i)
        arr(i) = arr(j)
        arr(j) = tmp
      }
      def partition(n: Int, r: Int, pivot: Int): Int = {
        val pivotVal = arr(pivot)
        swap(pivot, r)
        var j = n
        for (i <- n until r)
          if (arr(i) < pivotVal) {
            swap(i, j)
            j += 1
          }
        swap(j, r)
        j
      }
      def qs(n: Int, r: Int): Unit =
        if (n < r) {
          val pi = partition(n, r, n + (r - n)/2)
          qs(n, pi - 1)
          qs(pi + 1, r)
        }
      qs(0, arr.length - 1)
      arr.toList
    }

  def main(args: Array[String]): Unit = {
    println(quickSort(List(5, 1, 4, 3, 2)))
  }

}
