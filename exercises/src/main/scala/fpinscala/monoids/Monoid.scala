package fpinscala.monoids

import fpinscala.parallelism.ParFut._
// import fpinscala.parallelism.ParFut.toParOps // infix syntax for `Par.map`, `Par.flatMap`, etc
import language.higherKinds

trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

object Monoid {

  val stringMonoid = new Monoid[String] {
    def op(a1: String, a2: String) = a1 + a2
    val zero = ""
  }

  def listMonoid[A] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]) = a1 ++ a2
    val zero = Nil
  }

  val intAddition: Monoid[Int] = new Monoid[Int] {
    def op(a1: Int, a2: Int) = a1 + a2
    val zero = 0
  }

  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    def op(a1: Int, a2: Int) = a1 + a2
    val zero = 1
  }

  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean) = a1 || a2
    val zero = false
  }

  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean) = a1 && a2
    val zero = true
  }

  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    def op(a1: Option[A], a2: Option[A]) = a1.orElse(a2)
    val zero = None
  }

  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    def op(a1: A => A, a2: A => A): A => A = a2 compose a1
    val zero = (a: A) => a
  }

  def dual[A](m: Monoid[A]): Monoid[A] = new Monoid[A] {
    def op(x: A, y: A): A = m.op(y, x)
    val zero = m.zero
  }

  // TODO: Placeholder for `Prop`. Remove once you have implemented the `Prop`
  // data type from Part 2.
  trait Prop {}

  // TODO: Placeholder for `Gen`. Remove once you have implemented the `Gen`
  // data type from Part 2.

  import fpinscala.testing._
  import Prop._
//  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop = ???

 // def trimMonoid(s: String): Monoid[String] = ???

  //def concatenate[A](as: List[A], m: Monoid[A]): A =
   // ???

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.foldRight(m.zero){ case (a, acc) => m.op(f(a), acc) }

  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
    foldMap(as, endoMonoid[B])(a => b => f(a,b))(z)

  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
    foldMap(as, dual(endoMonoid[B]))(a => b => f(b,a))(z)

  def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B =
    if (as.length == 1) f(as.head)
    else if (as.isEmpty) m.zero
    else {
      val (half1, half2) = as.splitAt(as.length / 2)
      m.op(foldMapV(half1, m)(f), foldMapV(half2, m)(f))
    }

  def ordered(ints: IndexedSeq[Int]): Boolean = {
    sealed trait Ordered

    case class Ascending(min: Int, max: Int) extends Ordered
    case class Descending(max: Int, min: Int) extends Ordered
    case class Neutral(num: Int) extends Ordered
    case object Zero extends Ordered
    case object NotOrdered extends Ordered

    val OrderedMonoid = new Monoid[Ordered] {
      def op(a1: Ordered, a2: Ordered) = (a1, a2) match {
        case (NotOrdered, _) => NotOrdered
        case (_, NotOrdered) => NotOrdered
        case (Zero, a2) => a2
        case (a1, Zero) => a1
        case (Neutral(num1), Neutral(num2)) => 
          if (num1 < num2) Ascending(num1, num2)
          else if (num1 > num2) Descending(num1, num2)
          else Neutral(num1)
        case (Neutral(num), Ascending(min, max)) => if (num <= min) Ascending(num, max) else NotOrdered
        case (Ascending(min, max), Neutral(num)) => if (max <= num) Ascending(min, num) else NotOrdered
        case (Neutral(num), Descending(max, min)) => if (num >= max) Descending(num, min) else NotOrdered
        case (Descending(max, min), Neutral(num)) => if (num <= min) Descending(max, num) else NotOrdered
        case (Ascending(_,_), Descending(_,_)) => NotOrdered
        case (Descending(_,_), Ascending(_,_)) => NotOrdered
        case (Ascending(mn1, mx1), Ascending(mn2,mx2)) => if (mn2 >= mx1) Ascending(mn1, mx2) else NotOrdered
        case (Descending(mx1, mn1), Descending(mx2, mn2)) => if (mn1 >= mx2) Descending(mx1, mn2) else NotOrdered
      }

      val zero = Zero
    }
    foldMap(ints.toList, OrderedMonoid)(Neutral) match {
      case NotOrdered => false
      case _ => true
    }
  }

  sealed trait WC
  case class Stub(chars: String) extends WC
  case class Part(lStub: String, words: Int, rStub: String) extends WC

  def par[A](m: Monoid[A]): Monoid[ParFut[A]] =
    new Monoid[ParFut[A]] {
      def op(a1Fut: ParFut[A], a2Fut: ParFut[A]) =
        map2(a1Fut, a2Fut)(m.op(_,_))
      val zero = unit(m.zero)
    }

  def parFoldMap[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): ParFut[B] =
    foldMapV(v, par(m))(unit[B] _ compose f)

  val wcMonoid: Monoid[WC] = new Monoid[WC] {
    def op(w1: WC, w2: WC): WC = (w1, w2) match {
      case (Stub(c1), Stub(c2)) => Stub(c1 + c2)
      case (Stub(c1), Part(l2, count, r2)) => Part(c1 + l2, count, r2)
      case (Part(l1, count, r1), Stub(c2)) => Part(l1, count, r1 + c2)
      case (Part(l1, count1, r1), Part(l2, count2, r2)) => 
        Part(l1, count1 + (if ((r1 + l2).isEmpty) 0 else 1) + count2, r2)
    }
    val zero = Stub("")
  }

  def charToWC(c: Char): WC = 
    if (Vector(" ","\t","\n").contains(c)) Part("", 0, "") 
    else Stub(c.toString)

  def count(s: String): Int =
    foldMapV(s.toIndexedSeq, wcMonoid)(charToWC) match {
      case Stub(_) => 0
      case Part(_, count, _) => count
    }

  def productMonoid[A,B](A: Monoid[A], B: Monoid[B]): Monoid[(A, B)] =
    new Monoid[(A,B)] {
      def op(tup1: (A,B), tup2: (A,B)) = (tup1, tup2) match {
        case ((a1,b1), (a2,b2)) => (A.op(a1,a2), B.op(b1,b2))
      }
      val zero = (A.zero, B.zero)
    }

  //def functionMonoid[A,B](B: Monoid[B]): Monoid[A => B] = ???

  //def mapMergeMonoid[K,V](V: Monoid[V]): Monoid[Map[K, V]] = ???

  //def bag[A](as: IndexedSeq[A]): Map[A, Int] = ???
}

trait Foldable[F[_]] {
  import Monoid._

  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B =
    foldMap(as)(f.curried)(endoMonoid[B])(z)
  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B =
    foldMap(as)(a => b => f(b,a))(dual(endoMonoid[B]))(z)
  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    foldRight(as)(mb.zero)((a: A, b: B) => mb.op(f(a), b))
  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    foldLeft(as)(m.zero)(m.op)
  def toList[A](as: F[A]): List[A] =
    foldRight(as)(List.empty[A])(_ :: _)
}

object ListFoldable extends Foldable[List] {
  override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B) = as match {
    case x :: xs => f(x, foldRight(xs)(z)(f))
    case _ => z
  }
  override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B) = as match {
    case x :: xs => foldLeft(xs)(f(z, x))(f)
    case _ => z
  }
  override def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B =
    foldLeft(as)(mb.zero)((b: B, a: A) => mb.op(b, f(a)))
}

object IndexedSeqFoldable extends Foldable[IndexedSeq] {
  override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B) =
    as match {
      case xs if xs.isEmpty => z
      case _ => f(as.head, foldRight(as.tail)(z)(f))
    }
  override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B) =
    as match {
      case xs if xs.isEmpty => z
      case _ => foldLeft(as.tail)(z)(f)
    }
  override def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B =
    if (as.isEmpty) mb.zero
    else if (as.length == 1) f(as.head)
    else {
      val (half1, half2) = as.splitAt(as.length / 2)
      mb.op(foldMap(half1)(f)(mb), foldMap(half2)(f)(mb))
    }
}

object StreamFoldable extends Foldable[Stream] {
  override def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B) =
    as match {
      case x #:: xs => f(x, foldRight(xs)(z)(f))
      case _ => z
    }

  override def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B) =
    as match {
      case x #:: xs => foldLeft(xs)(f(z, x))(f)
      case _ => z
    }

  override def foldMap[A,B](as: Stream[A])(f: A => B)(mb: Monoid[B]): B =
    as match {
      case x #:: xs => mb.op(f(x), foldMap(xs)(f)(mb))
      case _ => mb.zero
    }
}

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object TreeFoldable extends Foldable[Tree] {
  import Monoid._
  override def foldMap[A, B](as: Tree[A])(f: A => B)(mb: Monoid[B]): B =
    as match {
      case Leaf(a) => f(a)
      case Branch(left, right) => mb.op(foldMap(left)(f)(mb), foldMap(right)(f)(mb))
      case _ => mb.zero
    }
  override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B) =
    foldMap(as)(a => b => f(b,a))(dual(endoMonoid[B]))(z)
  override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B) =
    foldMap(as)(f.curried)(endoMonoid[B])(z)
}

object OptionFoldable extends Foldable[Option] {
  override def foldMap[A, B](as: Option[A])(f: A => B)(mb: Monoid[B]): B =
    as match {
      case None => mb.zero
      case Some(a) => f(a)
    }
  override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B) =
    as match {
      case None => z
      case Some(a) => f(z, a)
    }
  override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B) =
    as match {
      case None => z
      case Some(a) => f(a, z)
    }
}

