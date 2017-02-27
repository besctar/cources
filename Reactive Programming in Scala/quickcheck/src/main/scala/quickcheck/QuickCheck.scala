package quickcheck


import scala.math._
import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  property("min1") = forAll {
    a: Int =>
      val h = insert(a, empty)
      findMin(h) == a
  }

  property("gen1") = forAll {
    (h: H) =>
      val m = if (isEmpty(h)) 0 else findMin(h)
      findMin(insert(m, h)) == m
  }

  property("smallestOfTwoElements") = forAll {
    (a: Int, b: Int) =>
      val h1 = insert(a, empty)
      val h2 = insert(b, h1)
      findMin(h2) == min(a, b)
  }

  property("insertDeleteElement") = forAll {
    a: Int =>
      val h1 = insert(a, empty)
      val h2 = deleteMin(h1)
      isEmpty(h2)
  }

  property("minOfMelding") = forAll {
    (h1: H, h2: H) =>
      val meldedMin = findMin(meld(h1, h2))
      val minOfEach = min(findMin(h1), findMin(h2))
      meldedMin == minOfEach
  }

  property("properLink") = {
    val h2 = insert(4, insert(3, empty))
    val h1 = insert(2, insert(1, empty))
    val h3 = meld(h1, h2)
    linearize(h3).corresponds(List(1, 2, 3, 4)) {
      _ == _
    }
  }

  property("properRemoveMin") = {
    val h2 = insert(5, insert(4, empty))
    val h1 = insert(3, insert(2, insert(1, empty)))
    val h3 = meld(h2, h1)
    val h4 = deleteMin(h3)
    findMin(h4) == 2
  }

  property("sortedSeq") = forAll {
    (h: H) =>
      val linearized = linearize(h)
      linearized.corresponds(linearized.sorted[Int]) {
        _ == _
      }
  }

  def linearize(h: H): List[Int] = {
    def linearizeAcc(h: H, acc: List[Int]): List[Int] =
      if (isEmpty(h))
        acc.reverse
      else
        linearizeAcc(deleteMin(h), findMin(h) :: acc)
    linearizeAcc(h, List.empty)
  }


  lazy val genHeap: Gen[H] = for {
    x <- arbitrary[Int]
    h <- oneOf(value(empty), genHeap)
  } yield insert(x, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)
}
