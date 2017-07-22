package chapter03

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def sum(ints: List[Int]): Int = ints match {
    case Nil         => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil          => 1
    case Cons(0.0, _) => 0.0
    case Cons(x, xs)  => x * product(xs)
  }

  def tail[A](l: List[A]): List[A] = l match {
    case Cons(_, t) => t
    case Nil        => throw new Exception("Empty list")
  }

  def setHead[A](a: A, l: List[A]): List[A] = l match {
    case Nil        => throw new Exception("Empty list")
    case Cons(_, t) => Cons(a, t)
  }

  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil        => a2
    case Cons(h, t) => Cons(h, append(t, a2))
  }

  /**
    * Not everything works out so nicely. Implement a function, init, that returns a List
    * consisting of all but the last element of a List. So, given List(1,2,3,4), init will
    * return List(1,2,3). Why can’t this function be implemented in constant time like
    * tail?
    *
    * Because the constructors of list (Nil and Cons) do have reference to the first
    * elements of a list (init) just to the last elements (tail), so this is the reason
    * the function init can not be implemented in constant time like the function tail.
    */
  def init[A](l: List[A]): List[A] = l match {
    case Nil          => throw new Exception("Empty list")
    case Cons(_, Nil) => Nil
    case Cons(h, t)   => Cons(h, init(t))
  }


  /**
    * Generalize tail to the function drop, which removes the first n elements from a list.
    * Note that this function takes time proportional only to the number of elements being
    * dropped—we don’t need to make a copy of the entire List.
    */
  def drop[A](l: List[A], n: Int): List[A] = {

    def loop[A](l: List[A], step: Int): List[A] = {
      if (step == 0) l
      else loop(tail(l), step - 1)
    }

    loop(l, n)
  }

  /**
    * Implement dropWhile, which removes elements from the List prefix as long as they match a predicate.
    */
  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Cons(h, t) if (f(h)) =>  dropWhile(t)(f)
    case _        => l
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(h, t) => f(h, foldRight(t, z)(f))
    }

  def foldRight2[A, B](as: List[A], z: B, n: A, r: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(h, _) if (h == n) => r
      case Cons(h, t) => f(h, foldRight(t, z)(f))
    }

  def sum2(l: List[Int]) =
    foldRight(l, 0)(_+_)

  def product2(l: List[Double]) =
    foldRight(l, 1.0)(_*_)

  def product3(l: List[Double]) =
    foldRight2(l, 1.0, 0.0, 0.0)(_*_)

  /**
    * Compute the length of a list using foldRight.
    */
  def length[A](as: List[A]): Int =
    foldRight(as, 0)((_,b) => 1 + b)


  /**
    * Our implementation of foldRight is not tail-recursive and will result in a StackOverflowError
    * for large lists (we say it’s not stack-safe). Convince yourself that this is the
    * case, and then write another general list-recursion function, foldLeft, that is tail-recursive,
    * using the techniques we discussed in the previous chapter.
    */
  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = {

    @annotation.tailrec
    def loop(as: List[A], acc: B): B =
      as match {
        case Nil        => acc
        case Cons(h, t) => loop(t, f(acc, h))
      }

    loop(as, z)
  }

  def sum3(l: List[Int]) =
    foldLeft(l, 0)(_+_)

  def product4(l: List[Double]) =
    foldLeft(l, 1.0)(_*_)

  /**
    * Compute the length of a list using foldLeft.
    */
  def length2[A](as: List[A]): Int =
    foldLeft(as, 0)((b,_) => 1 + b)

  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, List[A]())((acc,h) => Cons(h,acc))

  def reverse2[A](l: List[A]): List[A] = l match {
    case Nil        => l
    case Cons(h, t) => append(reverse2(t), List(h))
  }

  def concat[A](l: List[List[A]]): List[A] = foldRight(l, Nil:List[A])(append)

  def map[A, B](l: List[A])(f: A => B): List[B] =
    l match {
      case Nil => Nil
      case Cons(h, t) => Cons(f(h), map(t)(f))
    }

  def add_1(l: List[Int]): List[Int] =
    map(l)(_ + 1)

  def mapDoubleToString(l: List[Double]): List[String] = {
    map(l)(_.toString)
  }

  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    as match {
      case Nil                  => as
      case Cons(h, t) if (f(h)) => Cons(h, filter(t)(f))
      case Cons(_, t)           => filter(t)(f)
    }

  def removeOdd(l: List[Int]): List[Int] =
    filter(l)(_ % 2 == 0)

  /**
    * Write a function flatMap that works like map except that the function given will return
    * a list instead of a single result, and that list should be inserted into the final resulting
    * list. Here is its signature:
    *
    * For instance, flatMap(List(1,2,3))(i => List(i,i)) should result in List(1,1,2,2,3,3).
    */
  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] =
    as match {
      case Nil        => Nil
      case Cons(h, t) => append(f(h), flatMap(t)(f))
    }

  def filterViaFlatMap[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(a => if (f(a)) List(a) else Nil)

  def head[A](l: List[A]): A =
    l match {
      case Nil        => throw new Exception("Empty list")
      case Cons(h, _) => h
    }

  /**
    * Write a function that accepts two lists and constructs a new list by adding corresponding
    * elements. For example, List(1,2,3) and List(4,5,6) become List(5,7,9).
    */
  def zipWith[A, B, C](l: List[A], r: List[B], na: A, nb: B)(f: (A, B) => C): List[C] =
    l match {
      case Nil if (r == Nil)        => Nil
      case Nil                      => Cons(f(na, head(r)), zipWith(Nil, tail(r), na, nb)(f))
      case Cons(h, t) if (r == Nil) => Cons(f(h, nb), zipWith(t, Nil, na, nb)(f))
      case Cons(h, t)               => Cons(f(h, head(r)), zipWith(t, tail(r), na, nb)(f))
    }

  def add(l: List[Int], r: List[Int]): List[Int] =
    zipWith(l, r, 0, 0)(_ + _)

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {

    @annotation.tailrec
    def go(l: List[A], r: List[A], original: List[A]): Boolean =
      (l, r) match {
        case (Nil, Nil)                           => true
        case (Nil, _)                             => false
        case (_, Nil)                             => true
        case (Cons(h, t), Cons(g, s)) if (h == g) => go(t, s, original)
        case _                                    => go(tail(original), sub, tail(original))
      }

    go(sup, sub, sup)
  }





  /**
    * Variadic function, meaning it accepts zero or more arguments of type A.s
    *
    * Variadic functions are just providing a little syntactic sugar for creating and passing
    * a Seq of elements explicitly. Seq is the interface in Scala’s collections library implemented
    * by sequence-like data structures such as lists, queues, and vectors. Inside
    * apply, the argument as will be bound to a Seq[A] (documentation at http://mng.bz/f4k9),
    * which has the functions head (returns the first element) and tail (returns all
    * elements but the first). The special _* type annotation allows us to pass a Seq to a
    * variadic method.
    *
    * The operator :_* (tells the compiler that we're rather working with a varargs, than a sequence).
    * Particularly useful for the methods that can accept only varargs.
    *
    * The operator @ is used for Annotations and variable binding on pattern matching
    */
  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail:_*))
}

import List._

object ListApp {

  def main(args: Array[String]): Unit = {

    val x = List(1, 2, 3, 4, 5) match {

      case Cons(x, Cons(2, Cons(4, _)))          => x
      case Nil                                   => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t)                            => h + sum(t)
      case _                                     => 101

    }

    println(x)

    val list1 = List(1, 2, 3)

    println(tail(list1))

    val list2 = List(2, 3)

    println(tail(list2))

    val list3 = List(3)

    println(tail(list3))

    println(setHead(5, list1))
    println(setHead(5, list2))
    println(setHead(5, list3))

    println(drop(list1, 0))
    println(drop(list1, 1))
    println(drop(list1, 2))
    println(drop(list1, 3))

    println(dropWhile(list1)(a => a <= 2))

    println(init(list1))

    println(foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_)))

    println(length(list1))

    println(reverse(list1))

    println(reverse2(list1))

    println(concat(List(List(1,2,3), List(4,5), List(6))))

    println(add_1(List(1, 2, 3)))

    val strList: List[String] = mapDoubleToString(List(2.4, 3.5))

    println(strList)

    println(removeOdd(List(1,2,3,4,5,6,7,8,9,10)))

    val l = flatMap(List(1,2,3))(i => List(i,i))

    println(l)

    println(add(List(1,2,3), List(4,5,6)))

    println(hasSubsequence(List(1,2,3,4), List(1,2)))
    println(hasSubsequence(List(1,2,3,4), List(2,3)))
    println(hasSubsequence(List(1,2,3,4), List(4)))
    println(hasSubsequence(List(1,2,3,4), List(1,3)))
    println(hasSubsequence(List(1,2,3,4), Nil))

  }

}