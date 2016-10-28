package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

//abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {
//
//  object BinomialIntHeap extends BinomialHeap with IntHeap
//
//  lazy val genHeap: Gen[H] = for {
//    v <- Arbitrary.arbitrary[A]
//    m <- Gen.oneOf(Gen.const(empty), genHeap)
//  } yield insert(v, m)
//
//  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)
//
//  property("gen1") = Prop.forAll { (h: H) =>
//    val m = if (isEmpty(h)) 0 else findMin(h)
//    findMin(insert(m, h)) == m
//  }
//
//  property("sorted") = Prop.forAll { (h: H) =>
//    def isSorted(last: Int, heap: H): Boolean = {
//      isEmpty(heap) || {
//        val min = findMin(heap)
//        last <= min && isSorted(min, deleteMin(heap))
//      }
//    }
//
//    isEmpty(h) || isSorted(findMin(h), deleteMin(h))
//  }
//
//  property("minOfSome") = Prop.forAll { (h1: H, h2: H) =>
//    val m1 = if(isEmpty(h1)) 0 else findMin(h1)
//    val m2 = if(isEmpty(h2)) 0 else findMin(h2)
//    val m = findMin(meld(h1, h2))
//
//    m == m1 || m == m2
//  }
//}
abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    a <- arbitrary[A]
    h <- Gen.oneOf(Gen.const(empty), genHeap)
  } yield insert(a, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("insert") = forAll { (a: A) =>
    val h = insert(a, empty)
    val m = findMin(h)
    m == a
  }

  property("findMin") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("Meld") = forAll { (h: H) =>
    val h1 = meld(h, empty)
    val h2 = meld(empty, h)
    h1 == h2
  }

  property("Meld2") = forAll { (h1: H, h2: H) =>
    val h3 = meld(h1, h2)
    findMin(h3) == findMin(h1) || findMin(h3) == findMin(h2)
  }

  property("Delete") = forAll { (h: H) =>
    val m1 = if (isEmpty(h)) 0 else findMin(h)
    val h1 = deleteMin(h)
    val m2 = if (isEmpty(h1)) 0 else findMin(h1)
    val h2 = deleteMin(h1)
    findMin(insert(m2, insert(m1, h2))) == m1
  }

//  property("deleteMin1") = forAll { (h: H) =>
//    val h2 = deleteMin(h)
//    val h3 = deleteMin(h2)
//    val m1 = if (isEmpty(h)) 0 else findMin(h)
//    val m2 = if (isEmpty(h2)) 0 else findMin(h2)
//    findMin(insert(m2, insert(m1, h3))) == m1
//  }

//  property("meld1") = forAll { (h: H) =>
//    val m = if (isEmpty(h)) empty else meld(h, empty)
//    h == m
//  }
//
//  property("deleteMin1") = forAll { (h: H) =>
//    val m = if (isEmpty(h)) 0 else findMin(h)
//    val h_min = deleteMin(h)
//    val new_m = if (isEmpty(h)) 0 else findMin(h_min)
//    m != new_m
//  }

  //insert, meld, findMin, deleteMin

//  lazy val genMap: Gen[Map[Int,Int]] = for {
//    k <- arbitrary[Int]
//    v <- arbitrary[Int]
//    m <- oneOf(const(Map.empty[Int,Int]), genMap)
//  } yield m.updated(k, v)

//  property("min1") = forAll { a: Int =>
//    val h = insert(a, empty)
//    findMin(h) == a
//  }



//  // Irrelevant
//  property("empty") = forAll { (a: Int) =>
//    !isEmpty(insert(a, empty))
//  }
//
//  // Shows that Bogus 1 (One) and Bogus 2 (Two) are buggy
//  property("gen1") = forAll { (h: H) =>
//    val m = if (isEmpty(h)) 0 else findMin(h)
//    findMin(insert(m, h)) == m
//  }
//
//  // Irrelevant
//  property("insert two mins") = forAll { (a: Int, b: Int) =>
//    val h = insert(a, insert(b, empty)) // TODO: suchThat or implication
//  val m = findMin(h)
//    if (a > b) m == b else m == a
//  }
//
//  // Irrelevant
//  property("insert and delete") = forAll { (a: Int) =>
//    val h = insert(a, empty)
//    isEmpty(deleteMin(h))
//  }
//
//  def isOrdered(h: H): Boolean = {
//    def isOrderedHelper(x: Int, h: H): Boolean =
//      if (isEmpty(h)) true
//      else {
//        val m = findMin(h)
//        x <= m && isOrderedHelper(m, deleteMin(h))
//      }
//    if(isEmpty(h)) true else isOrderedHelper(findMin(h), deleteMin(h))
//  }
//
//  // This shows that Bogus 5 (Five) is buggy
//  property("heap sort") = forAll { (h: H) =>
//    isOrdered(h)
//  }
//
//  // Irrelevant
//  property("minimum of the meld") = forAll { (h1: H, h2: H) =>
//    val m1 = if (isEmpty(h1)) 0 else findMin(h1) // TODO: suchThat or implication
//  val m2 = if (isEmpty(h2)) 0 else findMin(h2)
//    val m = findMin(meld(insert(m1, h1), insert(m2, h2)))
//    m == m1 || m == m2
//  }
//
//  // Shows that Bogus 3 (three) is buggy
//  property("insert two mins and then delete") = forAll { (a: Int, b: Int) =>
//    val h = insert(a, insert(b, empty))
//    val fstMin = findMin(h)
//    val sndMin = findMin(deleteMin(h))
//    if (a > b) (fstMin == b) && (sndMin == a) else (fstMin == a) && (sndMin == b)
//  }
//
//  // Irrelevant
//  property("insert twice the same value") = forAll { (a: Int) =>
//    val h = insert(a, insert(a, empty))
//    val fstMin = findMin(h)
//    val sndMin = findMin(deleteMin(h))
//    (a == fstMin) && (a == sndMin) && isEmpty(deleteMin(deleteMin(h)))
//  }
//
//  // This shows that Bogus 4 (Four) is buggy
//  property("skinnycatting") = forAll { (a: Int, b: Int) =>
//    val h = insert(a, insert(a, insert(b, empty)))
//    //val fstMin = findMin(h)
//    val sndMin = findMin(deleteMin(h))
//    //val trdMin = findMin(deleteMin(deleteMin(h)))
//    (a == sndMin)
//    //if (a > b)
//    //  (b == fstMin) &&  (a == trdMin)
//    //else
//    //  (a == fstMin) && (b == trdMin)
//    //)
//  }
}

