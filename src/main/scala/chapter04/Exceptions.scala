package chapter04

object Exceptions {

  /**
    * We can prove that y is not referentially transparent. Recall that any RT expression may
    * be substituted with the value it refers to, and this substitution should preserve program
    * meaning. If we substitute throw new Exception("fail!") for y in x + y, it produces
    * a different result, because the exception will now be raised inside a try block
    * that will catch the exception and return 43.
    */
  def failingFn(i: Int): Int = {
    val y: Int = throw new Exception("fail!")

    try {
      val x = 42 + 5
      x + y
    } catch {
      case e: Exception => 43
    }
  }

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5

      // A thrown Exception can be given any type; here we’re annotating it with the type Int.
      x + ((throw new Exception("fail!")):Int)
    } catch {
      case _: Exception => 43
    }
  }

  def main(args: Array[String]): Unit = {
    println(failingFn2(12))
  }

}
