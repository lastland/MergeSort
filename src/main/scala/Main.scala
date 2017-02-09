import cats.implicits._

import MergeSort._

object Main {
  def main(args: Array[String]) {
    println(realMergeSort(100000 to 1 by -1 toList).last)
  }
}
