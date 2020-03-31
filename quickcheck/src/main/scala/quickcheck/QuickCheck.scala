package quickcheck

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = oneOf(
    const(empty),
    for {
      k <- arbitrary[Int]
      h <- oneOf(const(empty), genHeap)
    } yield insert(k, h)
  )

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  /**
    * If you insert any two elements into an empty heap,
    * finding the minimum of the resulting heap should get the smallest of the two elements back
    */
  property("min2") = forAll { (a: Int, b: Int) =>
    val h = insert(b, insert(a, empty))
    findMin(h) == Math.min(a, b)
  }

  /**
    * If you insert an element into an empty heap, then delete the minimum, the resulting heap should be empty.
    */
  property("emp") = forAll { (a: Int) =>
    val h = insert(a, empty)
    deleteMin(h) == empty
  }

  /**
    * finding a minimum of the melding of any two heaps should return a minimum of one or the other.
    */
  property("meld") = forAll { (h: H, h1: H) =>
    if (isEmpty(h) && isEmpty(h1))
      true
    else if (isEmpty(h) && !isEmpty(h1))
      findMin(meld(h, h1)) == findMin(h1)
    else if (!isEmpty(h) && isEmpty(h1))
      findMin(meld(h, h1)) == findMin(h)
    else
      findMin(meld(h, h1)) == Math.min(findMin(h), findMin(h1))
  }


  /**
    * melding of two heaps should contain elements from both
    */
  property("meld_contains_all") = forAll { (h: H, h1: H) =>
    def accum(l: List[Int], h: H): List[Int] = {
      if (isEmpty(h))
        l
      else
        accum(findMin(h) :: l, deleteMin(h))
    }

    val both = accum(accum(List.empty, h), h1)
    val mld = accum(List.empty, meld(h, h1))
    both.sorted == mld.reverse
  }

  /**
    * Given any heap, you should get a sorted sequence of elements when continually finding and deleting minima.
    * (Hint: recursion and helper functions are your friends.)
    */
  property("sort") = forAll { (h: H) =>
    def testHelp(prev: Int, h: H): Boolean = {
      if (isEmpty(h))
        true
      else {
        val min = findMin(h)
        min >= prev && testHelp(min, deleteMin(h))
      }
    }

    isEmpty(h) || testHelp(findMin(h), deleteMin(h))
  }

}
