object MergeSort {

  def mergeSort(v: Vector[Int]): Vector[Int] = {
    if (v.length <= 1) {
      v
    } else {
      val mid = v.length / 2

      val left = mergeSort(v.take(mid))
      val right = mergeSort(v.drop(mid))

      merge(left, right)
    }
  }

  def merge(left: Vector[Int], right: Vector[Int]): Vector[Int] = {
    var i = 0
    var j = 0
    var res = Vector[Int]()

    while (i < left.length && j < right.length) {
      if (left(i) <= right(j)) {
        res = res :+ left(i)
        i += 1
      } else {
        res = res :+ right(j)
        j += 1
      }
    }

    while (i < left.length) {
      res = res :+ left(i)
      i += 1
    }

    while (j < right.length) {
      res = res :+ right(j)
      j += 1
    }

    res
  }

  def main(args: Array[String]): Unit = {
    val v = Vector(5, 1, 4, 2, 8, 3, 3)
    val sorted = mergeSort(v)
    println(sorted)
  }
}
