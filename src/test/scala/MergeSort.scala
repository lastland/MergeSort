import org.scalacheck.Properties
import org.scalacheck.Prop.{forAll, classify, BooleanOperators}

import cats._
import cats.implicits._

import MergeSort._

object MergeSortSpecification extends Properties("MergeSort") {

  def isOrdered[A: Order](a: List[A]): Boolean = a match {
    case (x :: y :: zs) => x <= y && isOrdered(zs)
    case _ => true
  }

  property("isOrdered") = forAll { a: List[Int] =>
    isOrdered(realMergeSort(a))
  }

  property("idemp") = forAll { a: List[Int] =>
    realMergeSort(realMergeSort(a)) == realMergeSort(a)
  }

  property("min") = forAll { a: List[Int] =>
    !a.isEmpty ==> (realMergeSort(a).head == a.min)
  }

  property("max") = forAll { a: List[Int] =>
    !a.isEmpty ==> (realMergeSort(a).last == a.max)
  }

  property("sort") = forAll { a: List[Int] =>
    classify(a.length > 10, "large", "small") {
      realMergeSort(a) == a.sorted
    }
  }
}
