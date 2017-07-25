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
    flatMap(a => sb.flatMap( b => unit( f(a, b) ) ) )

}

object State {

  type Rand[A] = State[RNG, A]

  def unit[S, A](a: A): State[S, A] = State(run = s => (a, s))

  /**
    * Tail recursive implementation of sequence
    */
  def sequence[S, A](sas: List[State[S, A]]): State[S, List[A]] = {

    @annotation.tailrec
    def go(fs: List[State[S, A]], rs: State[S, List[A]]): State[S, List[A]] =
      fs match {
        case Nil    => rs
        case h :: t => go(t, cons(h, rs))
      }

    go(sas, unit(Nil))
  }

  def cons[S, A](r: State[S, A], rs: State[S, List[A]]): State[S, List[A]] =
    r.map2(rs)(_::_)

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get // Gets the current state and assigns it to `s`.
    _ <- set(f(s)) // Sets the new state to `f` applied to `s`.
  } yield ()

}

import State._

object StateApp {

  def main(args: Array[String]): Unit = {

    new Rand(s => {
      (1, s)
    })

    val s = modify[Int](s => s)

    val ns = s.run(1)

    println(ns._1)
    println(ns._2)

  }

}
