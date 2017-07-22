package chapter03

sealed trait Tree[+A]
case class Leaf[+A](value: A) extends Tree[A]
case class Branch[+A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  def size[A](tree: Tree[A]): Int =
    tree match {
      case Leaf(_)      => 1
      case Branch(l, r) => 1 + size(l) + size(r)
    }

  def max(tree: Tree[Int]): Int =
    tree match {
      case Leaf(v) => v
      case Branch(l, r) => max(l) max max(r)
    }

  def depth[A](tree: Tree[A]): Int =
    tree match {
      case Leaf(_)      => 0
      case Branch(l, r) => 1 + (depth(l) max depth(r))
    }

  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] =
    tree match {
      case Leaf(a)      => Leaf(f(a))
      case Branch(l, r) => Branch(map(l)(f), map(r)(f))
    }

  /**
    * Generalize size, maximum, depth, and map, writing a new function fold that abstracts over their similarities.
    * Reimplement them in terms of this more general function. Can
    * you draw an analogy between this fold function and the left and right folds for List?
    */
  def fold[A, B](t: Tree[A])(f: Leaf[A] => B)(g: (B, B) => B):B =
    t match {
      case l @ Leaf(_)  => f(l)
      case Branch(l, r) => {
        val vl = fold(l)(f)(g)
        val vr = fold(r)(f)(g)
        g(vl, vr)
      }
    }

  def sizeViaFold[A](tree: Tree[A]): Int =
    fold(tree)(_=> 1)((a, b) => 1 + a + b)

  def maxViaFold(tree: Tree[Int]): Int =
    fold(tree)(l => l.value)(_ max _)

  def depthViaFold[A](tree: Tree[A]): Int =
    fold(tree)(_ => 0)(1 + _ max _)

  def mapViaFold[A, B](tree: Tree[A])(f: A => B): Tree[B] =
    fold(tree)(l => Leaf(f(l.value)):Tree[B])(Branch(_, _))

}


import Tree._

object TreeApp {

  def main(args: Array[String]): Unit = {

    val tree = Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))

    println("Size.......: " + size(tree))
    println("SizeViaFold: " + sizeViaFold(tree))

    println("Max.......: " + max(tree))
    println("MaxViaFold: " + maxViaFold(tree))

    println("Depth.......: " + depth(tree))
    println("DepthViaFold: " + depthViaFold(tree))

    println("Map.......: " + map(tree)(2 * _))
    println("MaxViaFold: " + mapViaFold(tree)(2 * _))

  }

}