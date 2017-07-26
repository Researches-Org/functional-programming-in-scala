package chapter06

import State._

/**
  * Generalize the functions unit, map, map2, flatMap, and sequence. Add them as methods
  * on the State case class where possible. Otherwise you should put them in a State
  * companion object.
  */
case class State[S,+A](run: S => (A, S)) {

  def map[B](f: A => B): State[S, B] =
    flatMap(a => unit(f(a)))

  def flatMap[B](g: A => State[S, B]): State[S, B] =
    State(s => {
      val (a, st) = run(s)
      g(a).run(st)
    })

  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.map( b => f(a, b)  ) )

}

object State {

  type Rand[A] = State[RNG, A]

  def unit[S, A](a: A): State[S, A] = State(run = s => (a, s))

  // This implementation uses a loop internally and is the same recursion
  // pattern as a left fold. It is quite common with left folds to build
  // up a list in reverse order, then reverse it at the end.
  // (We could also use a collection.mutable.ListBuffer internally.)
  def sequence[S, A](sas: List[State[S, A]]): State[S, List[A]] = {
    def go(s: S, actions: List[State[S,A]], acc: List[A]): (List[A],S) =
      actions match {
        case Nil => (acc.reverse,s)
        case h :: t => h.run(s) match { case (a,s2) => go(s2, t, a :: acc) }
      }
    State((s: S) => go(s,sas,List()))
  }

  def cons[S, A](r: State[S, A], rs: State[S, List[A]]): State[S, List[A]] =
    r.map2(rs)(_::_)

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  def modify[S](f: S => S): State[S, Unit] =
    for {
      s <- get // Gets the current state and assigns it to `s`.
      _ <- set(f(s)) // Sets the new state to `f` applied to `s`.
    } yield ()


  def modify1[S](f: S => S): State[S, Unit] =
    get.flatMap(s => set(f(s)).map(_ => ()))

}

import State._

object StateApp {

  def main(args: Array[String]): Unit = {

    println(modify[Int](_))

    println(modify1[Int](_))

  }

}
