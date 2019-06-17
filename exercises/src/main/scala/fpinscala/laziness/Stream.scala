package fpinscala.laziness

import Stream._
trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def toList: List[A] =
    this match {
      case Empty => Nil
      case Cons(h, t) => h() :: t().toList
    }

  def take(n: Int): Stream[A] =
    this match {
      case Cons(h, t) if n > 0 => Cons(h, () => t().take(n-1))
      case _ => empty
    }

  def drop(n: Int): Stream[A] =
    this match {
      case Cons(h, t) if n > 0 => t().drop(n-1)
      case _ => this
    }

  def takeWhile(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, b) => if (p(a)) cons(a, b) else empty)

  def takeWhilePM(p: A => Boolean): Stream[A] =
    this match {
      case Cons(h, t) if p(h()) => Cons(h, () => t().takeWhile(p))
      case _ => empty
    }

  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((h,t) => cons(f(h), t))

  def filter(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((h, t) => if (p(h)) cons(h, t) else t)

  def append[B>:A](s: Stream[B]): Stream[B] =
    foldRight(s)(cons(_,_))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((h, t) => f(h).append(t))

  def forAll(p: A => Boolean): Boolean =
    this match {
      case Cons(h, t) => p(h()) && t().forAll(p)
      case _ => true
    }

  def headOption: Option[A] =
    this match {
      case Cons(h, _) => Some(h())
      case _ => None
    }

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def startsWith[B](s: Stream[B]): Boolean =
    (this, s) match {
      case (Cons(h1, t1), Cons(h2, t2)) => 
        if (h1() == h2()) t1().startsWith(t2())
        else false
      case (empty, Cons(_,_)) => false
      case _ => true
    }

  def startsWith2[B](s: Stream[B]): Boolean =
    this.zipAll(s).forAll {
      case (Some(a),Some(b)) => a == b
      case (Some(_),None) => true
      case _ => false
    }

  def mapUnfold[B](f: A => B): Stream[B] =
    unfold(this) {
      case Cons(h, t) => Some((f(h()), t()))
      case _ => None
    }

  def takeUnfold(n: Int): Stream[A] =
    unfold((n, this)) {
      case (x, Cons(h, t)) if x > 0 => Some((h(), (n-1, t())))
      case _ => None
    }

  def takeWhileUnfold(f: A => Boolean): Stream[A] =
    unfold(this) {
      case Cons(h, t) if f(h()) => Some((h(), t()))
      case _ => None
    }

  def zip[B](s: Stream[B]): Stream[(A,B)] =
    unfold((this, s)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some(((h1(), h2()), (t1(), t2())))
      case _ => None
    }

  def zipAll[B](s: Stream[B]): Stream[(Option[A], Option[B])] =
    unfold((this, s)) {
      case (Cons(h1, t1), Cons(h2, t2)) => 
        Some((
          (Some(h1()), Some(h2())), 
          (t1(), t2())
        ))
      case (empty, Cons(h, t)) => Some(((None, Some(h())), (empty, t())))
      case (Cons(h, t), empty) => Some(((Some(h()), None), (t(), empty)))
      case _ => None
    }

  def tails: Stream[Stream[A]] =
    cons(this, 
      unfold(this) {
        case Cons(_,t) => Some((t(), t()))
        case _ => None
      }
    )

  def tails2: Stream[Stream[A]] =
    unfold(this) {
      case Empty => None
      case s => Some((s, s.drop(1)))
    } append(Stream(empty))

  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
    foldRight((z, empty[B])) {
      case (a, (scanAcc, streamAcc)) => 
        lazy val scanAccUpdated = f(a, scanAcc)
        (scanAccUpdated, cons(scanAccUpdated, streamAcc))
    }._2
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)
  def constant[A](a: A): Stream[A] = cons(a, constant(a))
  def from(n: Int): Stream[Int] = cons(n, from(n+1))
  def fibs: Stream[Int] = {
    def helper(f0: Int, f1: Int): Stream[Int] = cons(f0, helper(f1, f0 + f1))
    helper(0,1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case Some((a, s)) => cons(a, unfold(s)(f))
      case None => empty[A]
    }

  def onesUnfold: Stream[Int] = unfold(1)(_ => Some((1, 1)))

  def constantUnfold[A](a: A): Stream[A] = unfold(a)(_ => Some((a,a)))

  def fromUnfold(n: Int): Stream[Int] = unfold(n)(a => Some((a,a+1)))

  def fibsUnfold: Stream[Int] = {
    def helper(f0: Int, f1: Int): Stream[Int] = cons(f0, helper(f1, f0+f1))
    helper(0,1)
  }

}
