package fpinscala.monoids

import org.scalatest.{FunSpec, Matchers}

class MonoidSpec extends FunSpec with Matchers {
  describe("Testing ListFoldable") {
    val list1 = List(1,2,3,4,5)
    val list2 = List(2,4,6,8,10)
    val list3 = List(10,8,6,4,2)

    it("should properly sum List of Int with foldRight") {
      ListFoldable.foldRight(list1)(0)(_ + _) shouldBe 15
    }
    it("should properly sum List of Int with foldLeft") {
      ListFoldable.foldLeft(list1)(0)(_ + _) shouldBe 15
    }
    it("should properly sum List of Int with foldMap") {
      ListFoldable.foldMap(list1)(identity)(Monoid.intAddition) shouldBe 15
    }
    it("should properly build new list with foldRight") {
      val actual = 
        ListFoldable.foldRight(list1)(List.empty[Int])(2*_ :: _)
      actual shouldBe list2
    }
    it("should properly build new list with foldLeft") {
      val actual = 
        ListFoldable.foldLeft(list1)(List.empty[Int])((b, a) => 2*a :: b)
      actual shouldBe list3
    }
    it("should properly convert to String and foldMap into new list") {
      val input = List(1,2,1,0,3)
      val expected = List("one", "not binary", "one", "zero", "not binary")
      val intoString = (i: Int) => i match {
        case 0 => "zero"
        case 1 => "one"
        case _ => "not binary"
      }

      val actual =
        ListFoldable.foldMap(input)(i => List(intoString(i)))(Monoid.listMonoid)

      actual shouldBe expected
    }
  }

  describe("Testing IndexedSeqFoldable") {
    import IndexedSeqFoldable._

    val seq1 = IndexedSeq(1,2,3,4,5)
    val seq2 = IndexedSeq(2,4,6,8,10)
    val seq3 = IndexedSeq(10,8,6,4,2)

    it("should properly implement all with foldMap") {
      def all[A](as: IndexedSeq[A])(p: A => Boolean): Boolean =
        foldMap(as)(p)(Monoid.booleanAnd)

      all(seq1)(_ % 2 == 0) shouldBe false
      all(seq2)(_ % 2 == 0) shouldBe true
      all(seq3)(_ % 2 == 0) shouldBe true
    }
    it("should properly build new seq with foldRight") {
      val actual = foldRight(seq1)(IndexedSeq.empty[Int])(2*_ +: _)
      actual shouldBe seq2
    }
    it("should properly build new seq with foldLeft") {
      val actual = foldLeft(seq1)(IndexedSeq.empty[Int])((b, a) => 2*a +: b)
      actual shouldBe seq3
    }
  }

  describe("Testing TreeFoldable") {
    import TreeFoldable._
    val tree1 = Branch(Leaf(1), Leaf(4))
    val tree2 = Branch(Branch(Leaf(1),Leaf(2)), Leaf(3))
    val tree3 = Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))
    val tree4 = Branch(Leaf(1), Branch(Leaf(5), Leaf(3)))

    it("should properly create a DFS List with foldLeft") {
      val expected = List(3,2,1)
      val actual = foldLeft(tree2)(List.empty[Int])((b,a) => a :: b)

      actual shouldBe expected
    }
    it("should properly create a DFS List with foldLeft - tree3") {
      val expected = List(3,2,1)
      val actual = foldLeft(tree3)(List.empty[Int])((b,a) => a :: b)

      actual shouldBe expected
    }
    it("should properly create a DFS List with foldRight") {
      val expected = List(1,2,3)
      val actual = foldRight(tree2)(List.empty[Int])(_ :: _)

      actual shouldBe expected
    }
    it("should properly sum numbers with foldMap") {
      val expected = 6
      val actual = foldMap(tree2)(identity)(Monoid.intAddition)

      actual shouldBe expected
    }
    it("should properly create a function to add an int to the tree sum") {
      def sumMult = 
        (tree: Tree[Int]) => 
          foldMap[Int, Int => Int](tree)(a => b => a + b)(Monoid.endoMonoid[Int])
      sumMult(tree2)(5) shouldBe 11
      sumMult(tree3)(2) shouldBe 8
      sumMult(tree4)(5) shouldBe 14
    }
  }

  describe("Testing OptionFoldable") {
    import OptionFoldable._

    it("should properly foldMap int to string") {
      val fun = (i: Int) => i match {
        case 1 => "one"
        case 0 => "zero"
        case _ => "non binary"
      }
      val someone = Some(1)

      foldMap(someone)(fun)(Monoid.stringMonoid) shouldBe "one"
      foldMap(None)(fun)(Monoid.stringMonoid) shouldBe ""
    }

    it("should foldLeft string option with string") {
      val someone = Some("one")
      val expected = "Some one"
      foldLeft(someone)("Some ")(_ + _) shouldBe expected
    }

    it("should foldRight string option with string") {
      val someone = Some("one")
      val expected = "Some one"
      foldLeft(someone)("Some ")(_ + _) shouldBe expected
    }
  }

  describe("Testing productMonoid") {
    it("should properly use the productMonoid to combine two monoids") {
      val monoid = Monoid.productMonoid(Monoid.booleanAnd, Monoid.intAddition)
      monoid.op((true, 3), (false, 5)) shouldBe (false, 8)
      monoid.zero shouldBe (true, 0)
    }
  }

  describe("Testing functionMonoid") {
    import Monoid._

    it("should properly construct a generic functionMonoid for A => Int functions") {
      def funMonoid[A] = functionMonoid[A, Int](Monoid.intAddition)
      val strFun1 = (s: String) => s.length
      val strFun2 = (s: String) => s.filterNot(_ == 'x').length
      val strFun3 = funMonoid.op(strFun1, strFun2)

      val countOddLeaves = (t: Tree[Int]) =>
        TreeFoldable.foldMap(t)((i: Int) => if (i%2 == 1) 1 else 0)(intAddition)

      val countEvenLeaves = (t: Tree[Int]) =>
        TreeFoldable.foldMap(t)((i: Int) => if (i%2 == 0) 1 else 0)(intAddition)

      val countLeaves = funMonoid.op(countOddLeaves, countEvenLeaves)

      val tree1 = Branch(Branch(Leaf(1), Branch(Leaf(2), Leaf(3))), Leaf(4))
      val tree2 = Branch(Leaf(22), Branch(Leaf(33), Leaf(44)))

      strFun3("axxxle") shouldBe 9
      countLeaves(tree1) shouldBe 4
      countOddLeaves(tree1) shouldBe 2
      countEvenLeaves(tree1) shouldBe 2
      countLeaves(tree2) shouldBe 3
      countOddLeaves(tree2) shouldBe 1
      countEvenLeaves(tree2) shouldBe 2
    }
  }

  describe("Testing mapMergeMonoid") {
    import Monoid._

    it("Should properly build a monoid that properly merges a map") {
      def monoid[A] = mapMergeMonoid[A,Int](intAddition)
      val m1 = Map("this" -> 3, "that" -> 2)
      val m2 = Map("this" -> 5, "other" -> 33)
      val expected = Map("this" -> 8, "that" -> 2, "other" -> 33)

      monoid.op(m1, m2) shouldBe expected
    }
  }

  describe("Testing bags") {
    it("should properly build a bag from an IndexedSeq") {
      val idxSeq = IndexedSeq("this", "this", "that", "this", "other", "that")
      val mapRes = Map("this" -> 3, "that" -> 2, "other" -> 1)

      Monoid.bag(idxSeq) shouldBe mapRes
    }
  }
}
