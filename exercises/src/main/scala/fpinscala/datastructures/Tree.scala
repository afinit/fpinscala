package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {
  val tree1 = Branch(Branch(Leaf(5), Branch(Leaf(1), Leaf(2))), Leaf(2))

  def size[A](node: Tree[A]): Int =
    node match {
      case Leaf(_) => 1
      case Branch(left, right) => 1 + size(left) + size(right)
    }

  def max(node: Tree[Int]): Int =
    node match {
      case Leaf(value) => value
      case Branch(left, right) => max(left) max max(right)
    }

  def depth[A](node: Tree[A]): Int =
    node match {
      case Leaf(_) => 0
      case Branch(left, right) => 1 + (depth(left) max depth(right))
    }

  def map[A,B](node: Tree[A])(f: A => B): Tree[B] =
    node match {
      case Leaf(value) => Leaf(f(value))
      case Branch(left, right) => Branch(map(left)(f), map(right)(f))
    }

  def fold[A,B](node: Tree[A])(leafFun: A => B, branchFun: (B,B) => B): B =
    node match {
      case Leaf(value) => leafFun(value)
      case Branch(left, right) => branchFun(fold(left)(leafFun, branchFun), fold(right)(leafFun, branchFun))
    }

  def sizeFold[A](node: Tree[A]): Int =
    fold(node)(_ => 1, (left: Int, right: Int) => 1 + left + right)

  def maxFold(node: Tree[Int]): Int =
    fold(node)(identity, (left: Int, right: Int) => left max right)

  def depthFold[A](node: Tree[A]): Int =
    fold(node)(_ => 0, (left: Int, right: Int) => 1 + (left max right))

  def mapFold[A,B](node: Tree[A])(f: A => B): Tree[B] =
    fold(node)(value => Leaf(f(value)), (left: Tree[B], right: Tree[B]) => Branch(left, right))

}
