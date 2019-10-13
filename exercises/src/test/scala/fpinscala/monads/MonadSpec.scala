package fpinscala.monads

import org.scalatest.{FunSpec, Matchers}

import fpinscala.state.State

class MonadSpec extends FunSpec with Matchers {
  describe("Tests for Monads") {
    it("runs sequence as expected for optionMonad") {
      import Monad.optionMonad
      val listOpt = List(Some(5), Some(4), Some(3))
      val optList = Some(List(5,4,3))

      optionMonad.sequence(listOpt) shouldBe optList
      optionMonad.sequence(None :: listOpt) shouldBe None
    }

    it("runs sequence as expected for listMonad") {
      import Monad.listMonad
      val input = List(
        List(0,1),
        List(2,3),
        List(4,5)
      )
      val expected = List(
        List(0,2,4),
        List(0,2,5),
        List(0,3,4),
        List(0,3,5),
        List(1,2,4),
        List(1,2,5),
        List(1,3,4),
        List(1,3,5)
      )
      listMonad.sequence(input) shouldBe expected
    }

    it("runs traverse as expected for optionMonad") {
      import Monad.optionMonad
      val xs = List(1,2,3)
      val expected = Some(List(2,4,6))

      def mulOpt(optInt: Option[Int])(x: Int): Option[Int] =
        optInt.map(_ * x)

      optionMonad.traverse(xs)(mulOpt(Some(2))) shouldBe expected
      optionMonad.traverse(xs)(mulOpt(None)) shouldBe None
    }

    it("properly runs replicateM") {
      import Monad.optionMonad

      optionMonad.replicateM(3, Some(5)) shouldBe Some(List(5,5,5))
      optionMonad.replicateM(3, None) shouldBe None
    }

    it("properly runs filterM") {
      import Monad.optionMonad
      val xs = List(1,2,3)
      def theFilter(x: Int): Option[Boolean] =
        Some(x % 2 == 1)

      optionMonad.filterM(xs)(theFilter) shouldBe Some(List(1,3))
    }

    it("properly composes unit for optionMonad") {
      import Monad.optionMonad.{compose,unit}

      val composed1 = compose[Int,Int,Int](Some(_), unit[Int](_))
      val composed2 = compose[Int,Int,Int](unit[Int](_), Some(_))
      composed1(5) shouldBe Some(5)
      composed2(5) shouldBe Some(5)
    }

    it("properly uses idMonad") {
      import Monad.idMonad

      val xs = List(1,2,3,4,5)

      def isOdd(x: Int): Id[Boolean] =
        Id(x % 2 == 1)

      idMonad.traverse(xs)(isOdd) shouldBe Id(List(true,false,true,false,true))
      idMonad.filterM(xs)(isOdd) shouldBe Id(List(1,3,5))
    }

    it("properly uses stateMonad with traverse") {
      import Monad.stateMonad

      val expected = (List(4,7,10,8), 7)

      val xs = List(1,3,5,2)
      def stateSum(a: Int): State[Int,Int] =
        State(b => (a+b, b+1))

      val actual = stateMonad[Int].traverse(xs)(stateSum).run(3)

      actual shouldBe expected
    }

    it("should properly compose StateMonads") {
      import Monad.stateMonad

      def fib(a: Int, b: Int): Int = a+b
      val state: State[(Int,Int),Int] = State(b => (b._2, (b._2, fib(b._1,b._2))))

      def stateSum(a: Int): State[(Int,Int),Double] =
        state.map(_ + a)

      def stateMultiplier(a: Double): State[(Int,Int),Double] =
        state.map(_ * a)

      // This takes a starting number 4, adds that to the initial number
      // given in the fibonacci sequence 8, then multiplies that by
      // the next number in the fibonacci sequence 13
      // The final state is (13,21) as the next fib number is 21
      stateMonad[(Int,Int)]
        .compose(stateSum, stateMultiplier)(4).run((5,8)) shouldBe(156.0,(13,21))
    }

    it("should properly use the idMonad") {
      import Monad.idMonad

      val id1 = Id("Monad ")
      val id2 = Id("Life")

      val fmfmResult = idMonad.flatMap(id1) {
        a => idMonad.flatMap(id2) {
          b => Id(a + b)
        }
      }

      val fmmResult = idMonad.flatMap(id1) {
        a => idMonad.map(id2) {
          b => a + b
        }
      }

      fmfmResult shouldBe fmmResult
      fmfmResult shouldBe Id("Monad Life")
    }
  }
}
