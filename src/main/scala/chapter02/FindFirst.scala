package chapter02

object FindFirst {

  def findFirst(ss: Array[String], key: String): Int = {
    @annotation.tailrec
    def loop(n: Int): Int = {
      if (n >= ss.length) -1
      else if (ss(n) == key) n
      else loop(n + 1)
    }
    loop(0)
  }

  def findFirst[A](as: Array[A], p: A => Boolean): Int = {
    @annotation.tailrec
    def loop(n: Int): Int = {
      if (n >= as.length) -1
      else if(p(as(n))) n
      else loop(n + 1)
    }
    loop(0)
  }

  def main(args: Array[String]): Unit = {
    println(findFirst[String](Array("a", "b", "c"), a => a == "b"))
    println(findFirst[Int](Array(1, 2, 3), a => a == 3))
  }

}
