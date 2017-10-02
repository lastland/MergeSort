import cats._
import cats.data._
import cats.implicits._

import SortedList._

case class DivideList[A](list: List[A]) {
  def divide: (DivideList[A], DivideList[A]) = {
    val x = list.splitAt(list.length / 2)
    (DivideList(x._1), DivideList(x._2))
  }
}

object DivideList {
  /*
   We provide a foldable instance for DivideList.
   */
  implicit val divideListFoldable: Foldable[DivideList] =
    new Foldable[DivideList] {
      override def foldMap[A, B: Monoid](fa: DivideList[A])(f: A => B): B =
        fa.divide match {
          case (DivideList(List()), DivideList(bs)) =>
            // Because of the way we divide the list, `bs` can only be
            // an empty or singleton list in this case, so it's fine
            // to foldMap on the list. The following code also shows a
            // syntactic sugar in Scala, it is equivalent to:
            // bs.foldMap(f).
            bs foldMap f
          case (as, bs) =>
            // this is equivalent to as.foldMap(f).combine(bs.foldMap(f))
            (as foldMap f) |+| (bs foldMap f)
        }

      /*
       The following two methods are irrelevant to our merge sort.
       However, cats require us to provide some implementation
       for these two methods.
       */
      def foldLeft[A, B](fa: DivideList[A],b: B)(f: (B, A) => B): B =
        fa.list.foldLeft(b)(f)
      def foldRight[A, B](fa: DivideList[A],lb: Eval[B])
        (f: (A, Eval[B]) => Eval[B]): Eval[B] =
        fa.list.foldRight(lb)(f)
    }

  def foldSort[A: Order](xs: DivideList[A]): List[A] =
    // this is equivalent to xs.foldMap(singleton).toNormallist
    xs foldMap singleton toNormalList
}

object MergeSort {
  def realMergeSort[A: Order](xs: List[A]): List[A] =
    // this is equivalent to DivideList.foldSort(DivideList(xs))
    DivideList foldSort DivideList(xs)
}
