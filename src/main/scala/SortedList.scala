import cats._
import cats.data._
import cats.implicits._
import scala.annotation.tailrec

/*
 A trait cannot be directly instantiated.
 A sealed trait cannot be extended outside this file.
 */
sealed trait SortedList[A] {
  def toNormalList: List[A]
}

object SortedList {
  /*
   This is private so it cannot be called from outside.
   */
  private case class SortedListImpl[A](a: List[A]) extends SortedList[A] {
    def toNormalList = a
  }

  /*
   The only way to create something of type `SortedList` from outside
   is using the following method:
   */
  def singleton[A](a: A): SortedList[A] = SortedListImpl(List(a))

  /*
   In Scala, we declare an instance of a type class by using implicits.
   Here, we define a monoid instance for `SortedList`.
   For more information about Scala implicits, check:
   http://www.artima.com/pins1ed/implicit-conversions-and-parameters.html
   */
  implicit def sortedListMonoid[A: Order]: Monoid[SortedList[A]] =
    new Monoid[SortedList[A]] {
      // mempty is called empty
      def empty = SortedListImpl(List[A]())
      // mappend is called combine
      def combine(a: SortedList[A], b: SortedList[A]) =
        (a, b) match {
          // pattern matching is supported in Scala
          case (SortedListImpl(x), SortedListImpl(y)) =>
            SortedListImpl(merge(x, y))
        }
    }

  /*
   A naive implementation of `merge` would cause a StackOverflowException.
   We use a tail recursive version to deal with this problem.
   */
  private def merge[A: Order](a: List[A], b: List[A]): List[A] =
    mergeAux(a, b, Nil)

  @tailrec
  private def mergeAux[A: Order](a: List[A], b: List[A], c: List[A]): List[A] =
    (a, b) match {
      case (x :: xs, y :: ys) =>
        if (x < y) mergeAux(xs, b, x :: c)
        else mergeAux(a, ys, y :: c)
      case (Nil, ys) => c.reverse ++ ys
      case (xs, Nil) => c.reverse ++ xs
    }
}
