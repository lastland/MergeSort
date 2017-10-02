import org.scalacheck.Properties
import org.scalacheck.Prop.{forAll, classify, BooleanOperators}

import cats._
import cats.implicits._

import MergeSort._

object MergeSortSpecification extends Properties("MergeSort") {

  property("idemp") = forAll { a: List[Int] =>
    val b = realMergeSort(a)
    realMergeSort(b) == b
  }

  property("min") = forAll { (a: List[Int], b: Int) =>
    val l = b :: a
    realMergeSort(l).head == l.min
  }

  property("max") = forAll { (a: List[Int], b: Int) =>
    val l = b :: a
    realMergeSort(l).last == l.max
  }

  property("sort") = forAll { a: List[Int] =>
    classify(a.length > 10, "large", "small") {
      realMergeSort(a) == a.sorted
    }
  }
}
