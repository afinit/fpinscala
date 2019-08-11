package fpinscala.parallelism

import scala.concurrent._
import scala.concurrent.duration._

object ParFut {
  val defaultTimeout: Duration = 10.minutes
  type ParFut[A] = ExecutionContext => Future[A]

  def run[A](ec: ExecutionContext)(a: ParFut[A]): Future[A] = a(ec)

  def unit[A](a: A): ParFut[A] = (_: ExecutionContext) => Future.successful(a) // `unit` is represented as a function that returns a `UnitFuture`, which is a simple implementation of `Future` that just wraps a constant value. It doesn't use the `ExecutorService` at all. It's always done and can't be cancelled. Its `get` method simply returns the value that we gave it.

  def map2[A,B,C](a: ParFut[A], b: ParFut[B])(f: (A,B) => C): ParFut[C] = // `map2` doesn't evaluate the call to `f` in a separate logical thread, in accord with our design choice of having `fork` be the sole function in the API for controlling parallelism. We can always do `fork(map2(a,b)(f))` if we want the evaluation of `f` to occur in a separate thread.
    (ec: ExecutionContext) => {
      implicit val ecImplicit = ec
      val aFut = a(ec)
      val bFut = b(ec)
      for {
        aRes <- aFut
        bRes <- bFut
      } yield f(aRes, bRes)
    }

  def fork[A](a: => ParFut[A]): ParFut[A] = // This is the simplest and most natural implementation of `fork`, but there are some problems with it--for one, the outer `Callable` will block waiting for the "inner" task to complete. Since this blocking occupies a thread in our thread pool, or whatever resource backs the `ExecutionContext`, this implies that we're losing out on some potential parallelism. Essentially, we're using two threads when one should suffice. This is a symptom of a more serious problem with the implementation, and we will discuss this later in the chapter.
    ec => Future(a(ec))(ec).flatten

  def lazyUnit[A](a: => A): ParFut[A] = fork(unit(a))

  def map[A,B](pa: ParFut[A])(f: A => B): ParFut[B] = 
    map2(pa, unit(()))((a,_) => f(a))

  def join[A](a: ParFut[ParFut[A]]): ParFut[A] =
    ec => run(ec)(a).flatMap(run(ec))(ec)

  def joinFlatMap[A](a: ParFut[ParFut[A]]): ParFut[A] =
    flatMap(a)(x => x)

  def flatMap[A,B](pa: ParFut[A])(f: A => ParFut[B]): ParFut[B] =
    ec => run(ec)(pa).flatMap(f(_)(ec))(ec)

  def flatMapJoin[A,B](pa: ParFut[A])(f: A => ParFut[B]): ParFut[B] =
    join(map(pa)(f))

  def sortPar(parList: ParFut[List[Int]]) = map(parList)(_.sorted)

  // NOTE:: BLOCKING
  def equal[A](ec: ExecutionContext, timeout: Duration = defaultTimeout)(p: ParFut[A], p2: ParFut[A]): Boolean =
    Await.result(run(ec)(map2(p, p2)(_ == _)), timeout)

  def equal[A](p: ParFut[A], p2: ParFut[A]): ParFut[Boolean] =
    map2(p, p2)(_ == _) 

  def delay[A](fa: => ParFut[A]): ParFut[A] = 
    ec => fa(ec)

  // NOTE:: BLOCKING
  def choice[A](cond: ParFut[Boolean])(t: ParFut[A], f: ParFut[A]): ParFut[A] =
    ec => 
      run(ec)(cond).flatMap( c => if (c) t(ec) else f(ec) )(ec)

  def choiceN[A](n: ParFut[Int])(choices: List[ParFut[A]]): ParFut[A] =
    ec => run(ec)(n).flatMap(i => run(ec)(choices(i)))(ec)

  def choice2[A](cond: ParFut[Boolean])(t: ParFut[A], f: ParFut[A]): ParFut[A] =
    choiceN(map(cond)(b => if (b) 1 else 0))(List(f, t))

  def choiceNFM[A](n: ParFut[Int])(choices: List[ParFut[A]]): ParFut[A] =
    flatMap(n)(idx => choices(idx))

  def choiceMap[K,V](key: ParFut[K])(choices: Map[K,ParFut[V]]): ParFut[V] =
    flatMap(key)(choices)

  def chooser[A,B](pa: ParFut[A])(choices: A => ParFut[B]): ParFut[B] =
    flatMap(pa)(choices)

  def sequence[A](ps: List[ParFut[A]]): ParFut[List[A]] =
    if (ps.isEmpty) unit(Nil)
    else map2(ps.head, sequence(ps.tail))(_ :: _)

  def sequence2[A](ps: List[ParFut[A]]): ParFut[List[A]] =
    if (ps.isEmpty) unit(Nil)
    else map2(sequence(ps.take(ps.length / 2)), sequence(ps.drop(ps.length / 2)))(_ ++ _)

  def asyncF[A,B](f: A => B): A => ParFut[B] =
    a => lazyUnit(f(a))

  def parFilter[A](ps: List[A])(f: A => Boolean): ParFut[List[A]] =
    if (ps.isEmpty) lazyUnit(List.empty[A])
    else
      map2(
        lazyUnit(f(ps.head)),
        parFilter(ps.tail)(f)
      )(
        (predicate, tail) => if (predicate) ps.head :: tail else tail
      )

  def parFilter2[A](ps: List[A])(f: A => Boolean): ParFut[List[A]] = {
    val psFiltered = ps.map(asyncF(a => if (f(a)) List(a) else List()))
    map(sequence2(psFiltered))(_.flatten)
  }

  /* Gives us infix syntax for `ParFut`. */
  implicit def toParFutOps[A](p: ParFut[A]): ParFutOps[A] = new ParFutOps(p)

  class ParFutOps[A](p: ParFut[A]) {


  }
}

object ExamplesParFut {
  import ParFut._
  import java.util.concurrent.{ ExecutorService, Executors }
  val pool: ExecutorService = Executors.newFixedThreadPool(4)
  val ec: ExecutionContext = ExecutionContext.fromExecutorService(pool)

  def sum(ints: IndexedSeq[Int]): Int = // `IndexedSeq` is a superclass of random-access sequences like `Vector` in the standard library. Unlike lists, these sequences provide an efficient `splitAt` method for dividing them into two parts at a particular index.
    if (ints.size <= 1)
      ints.headOption getOrElse 0 // `headOption` is a method defined on all collections in Scala. We saw this function in chapter 3.
    else { 
      val (l,r) = ints.splitAt(ints.length/2) // Divide the sequence in half using the `splitAt` function.
      sum(l) + sum(r) // Recursively sum both halves and add the results together.
    }

  val ps1 = List(ParFut.unit(5), ParFut.unit(6), ParFut.unit(7))
  val ps1Run = ParFut.run(ec)(ParFut.sequence(ps1))
  val xs = List(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)

  // This demonstrates that parFilter is indeed running in parallel
  def filterMethod(x: Int) = {
    Thread.sleep(new scala.util.Random().nextInt(1500))
    println(x)
    x % 2 == 0
  }
  val xsFiltered = ParFut.parFilter(xs)(filterMethod)
  val xsFiltered2 = ParFut.parFilter2(xs)(filterMethod)
  // ParFut.run(pool)(xsFiltered)

}
