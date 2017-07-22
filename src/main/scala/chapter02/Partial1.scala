package chapter02

object Partial1 {

  //def partial1[A, B, C](a: A, f: (A, B) => C): B => C = f(a, _)
  def partial1[A, B, C](a: A, f: (A, B) => C): B => C = b => f(a, b)

}
