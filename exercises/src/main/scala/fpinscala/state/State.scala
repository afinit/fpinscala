package fpinscala.state


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, rng2) = rng.nextInt
    val iPos = i match {
      case _ if i >= 0 => i
      case Int.MinValue => Int.MaxValue
      case _ => i * -1
    }
    (iPos, rng2)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (iPos, rng2) = nonNegativeInt(rng)
    val iPosCapped = if (iPos == Int.MaxValue) iPos - 1 else iPos
    (iPosCapped.toDouble / Int.MaxValue, rng2)
  }

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i1, rng2) = rng.nextInt
    val (d1, rng3) = double(rng2)
    ((i1, d1), rng3)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val ((i1, d1), rng2) = intDouble(rng)
    ((d1, i1), rng2)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, rng2) = double(rng)
    val (d2, rng3) = double(rng2)
    val (d3, rng4) = double(rng3)
    ((d1, d2, d3), rng4)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) =
    (0 until count).foldLeft((List.empty[Int], rng)) {
      case ((randoms, rngAcc), _) => {
        val (i, rngNext) = rngAcc.nextInt
        (i :: randoms, rngNext)
      }
    }

  def intsRec(count: Int)(rng: RNG): (List[Int], RNG) = {
    def helper(count: Int, randoms: List[Int], rng2: RNG): (List[Int], RNG) = {
      if (count <= 0) (randoms, rng2)
      else {
        val (i, rngNext) = rng2.nextInt
        helper(count-1, i :: randoms, rngNext)
      }
    }
    helper(count, List.empty, rng)
  }

  def positiveMax(n: Int): Rand[Int] =
    map(nonNegativeInt)(rand => (rand / Int.MaxValue.toDouble * n).toInt)

  def doubleMap: Rand[Double] = map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = 
    rng => {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a,b), rng3)
    }

  def intDoubleMap2: Rand[(Int, Double)] =
    map2(_.nextInt, double)((_,_))

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    if (fs.length == 0) rng => (List.empty, rng)
    else map2(fs.head, sequence(fs.tail))(_ :: _)

  def intsSequence(count: Int)(rng: RNG): (List[Int], RNG) =
    sequence(List.fill(count)(int))(rng)

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (a, rng2) = f(rng)
      g(a)(rng2)
    }

  def mapFM[A,B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => unit(f(a)))

  def map2FM[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(
        rng => {
          val (a, r2) = ra(rng)
          val (b, r3) = rb(r2)
          ((a,b), r3)
        }
      )({ case (a,b) => unit(f(a,b)) })

  def map2FM2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A,B) => C): Rand[C] =
    flatMap(ra)(a => map(rb)(b => f(a,b)))

}

case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    State(
      s1 => {
        val (a, s2) = run(s1)
        (f(a), s2)
      }
    )

  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    State(
      s1 => {
        val (a, s2) = run(s1)
        val (b, s3) = sb.run(s2)
        (f(a,b), s3)
      }
    )

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State(
      s1 => {
        val (a, s2) = run(s1)
        f(a).run(s2)
      }
    )

}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]
  def unit[S,A](a: A): State[S, A] = State(s => (a, s))
  def sequence[S,A](fs: List[State[S,A]]): State[S,List[A]] =
    if (fs.isEmpty) unit(List.empty[A])
    else fs.head.map2(State.sequence(fs.tail))(_ :: _)

  // sequence via foldleft
  def sequenceFL[S,A](fs: List[State[S,A]]): State[S,List[A]] =
    fs.reverse.foldLeft(unit[S,List[A]](List()))((acc, s) => s.map2(acc)(_ :: _))

  def get[S]: State[S,S] = State(s => (s,s))
  def set[S](s: S): State[S,Unit] = State(_ => ((), s))

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
    def processInput(in: Input): State[Machine, (Int,Int)] =
      State(
        m => (m, in) match {
          case (mach @ Machine(l, 0, coin), _) => ((0, coin), mach)
          case (Machine(true, cand, coin), Coin) => ((cand, coin+1), Machine(false, cand, coin+1))
          case (Machine(false, cand, coin), Coin) => ((cand, coin), Machine(false, cand, coin))
          case (Machine(true, cand, coin), Turn) => ((cand, coin), Machine(true, cand, coin))
          case (Machine(false, cand, coin), Turn) => ((cand-1, coin), Machine(true, cand-1, coin))
        }
      )

    State( m => {
      val (l, m1) = State.sequence(inputs.map(processInput)).run(m)
      (l.last, m1)
    })
  }
}
