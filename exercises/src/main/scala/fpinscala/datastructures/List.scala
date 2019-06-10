package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  val l1 = List(1,2,3,4,5,6)
  val l2 = List(11,22,33,44,55,66)
  val l3 = List(1,3,5,7,9)

  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = l match {
    case Cons(_, t) => t
    case Nil => throw new IllegalArgumentException("Empty List does not have a tail")
  }

  def setHead[A](l: List[A], h: A): List[A] = Cons(h, tail(l))

  def drop[A](l: List[A], n: Int): List[A] = {
    if(n == 0) l
    else drop(tail(l), n-1)
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(h, t) if f(h) => dropWhile(t, f)
    case _ => l
  }

  def init[A](l: List[A]): List[A] = l match {
    case Cons(h, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
    case Nil => throw new IllegalArgumentException("Empty List does not have an init")
  }

  def length[A](l: List[A]): Int = foldRight(l, 0)((_, acc) => 1 + acc)

  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B =
    l match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }

  def sumFold(l: List[Double]): Double = foldLeft(l, 0.0)(_+_)
  def productFold(l: List[Double]): Double = foldLeft(l, 1.0)(_*_)
  def lengthFold[A](l: List[A]): Int = foldLeft(l, 0) ((acc, _) => 1 + acc)

  def reverse[A](l: List[A]): List[A] = foldLeft(l, Nil: List[A]) ((acc, a) => Cons(a, acc))

  def foldLeftFromFR[A,B](l: List[A], z: B)(f: (B, A) => B) =
    foldRight(l, (b: B) => b)((a,g) => b => g(f(b, a)))(z)

  def foldRightFromFL[A,B](l: List[A], z: B)(f: (A, B) => B) =
    foldLeft(l, (b: B) => b)((g,a) => b => g(f(a,b)))(z)

  def appendFold[A](a1: List[A], a2: List[A]): List[A] = {
    foldRight(a1, a2)(Cons(_,_))
  }

  def concatLists[A](lists: List[List[A]]): List[A] =
    foldRight(lists, Nil: List[A])(appendFold)

  def add1(l: List[Int]): List[Int] = 
    l match {
      case Nil => Nil
      case Cons(x, xs) => Cons(x+1, add1(xs))
    }

  def dblToStringMap(l: List[Double]): List[String] = {
    l match {
      case Nil => Nil
      case Cons(x, xs) => Cons(x.toString, dblToStringMap(xs))
    }
  }

  def map[A,B](l: List[A])(f: A => B): List[B] = 
    l match {
      case Nil => Nil
      case Cons(x, xs) => Cons(f(x), map(xs)(f))
    }
  
  def filter[A](l: List[A])(p: A => Boolean): List[A] = 
    l match {
      case Nil => Nil
      case Cons(x, xs) if p(x) => Cons(x, filter(xs)(p))
      case Cons(x, xs) => filter(xs)(p)
    }

  def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] =
    l match {
      case Nil => Nil
      case Cons(x, xs) => appendFold(f(x), flatMap(xs)(f))
    }

  def filterFlatMap[A](l: List[A])(p: A => Boolean): List[A] =
    flatMap(l)(x => if (p(x)) List(x) else Nil)

  def zipMap[A,B](l1: List[A], l2: List[A])(f: (A,A) => B): List[B] = 
    (l1, l2) match {
      case (_,_) if l1 == Nil || l2 == Nil => Nil
      case (Cons(x, xs), Cons(y, ys)) => Cons(f(x,y), zipMap(xs,ys)(f))
    }
}
