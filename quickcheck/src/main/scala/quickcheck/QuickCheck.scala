package quickcheck

import java.lang.Double

import common._
import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    n <- arbitrary[A]
    h <- frequency( (1, Gen.const(empty)),
                    (9, genHeap))
  } yield insert(n, h)


  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }


  property("gen2") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    if(m == Int.MaxValue)
      true
    else
      findMin(insert(m+1, h)) == m
  }

  property("gen3") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    if(m == Int.MinValue)
      true
    else
      findMin(insert(m-1, h)) == m-1
  }

  property("del1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    if (m == Int.MinValue){
      true
    }
    else {
      findMin(deleteMin(insert(m - 1, h))) == findMin(h)
    }
  }

  property("del2") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    if(m == Int.MinValue)
      true
    else
      findMin(deleteMin(insert(m-1, h))) == m
  }

  property("del3") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(deleteMin(insert(m, h))) == m
  }

  /* This took too much time. Interesting post!
  https://class.coursera.org/reactive-001/forum/thread?thread_id=97#post-371 */
  property("meld") = forAll { (h1: H, h2: H) =>
    def heapEqual(h1: H, h2: H): Boolean =
      if (isEmpty(h1) && isEmpty(h2)) true
      else {
        val m1 = findMin(h1)
        val m2 = findMin(h2)
        m1 == m2 && heapEqual(deleteMin(h1), deleteMin(h2))
      }

    heapEqual(meld(h1, h2), meld(deleteMin(h1), insert(findMin(h1), h2)))
  }
}
