object UniqueMergeSort {

  def mergeSort(v: Vector[Int]): Vector[Int] = {
    if (v.length <= 1) {
      v
    } else {
      val mid = v.length / 2

      val left = mergeSort(v.take(mid))
      val right = mergeSort(v.drop(mid))

      mergeUnique(left, right)
    }
  }

  def mergeUnique(left: Vector[Int], right: Vector[Int]): Vector[Int] = {
    var i = 0
    var j = 0
    var res = Vector[Int]()

    while (i < left.length && j < right.length) {
      if (left(i) < right(j)) {
        if (res.isEmpty || res.last != left(i)) {
          res = res :+ left(i)
        }
        i += 1
      } else if (left(i) > right(j)) {
        if (res.isEmpty || res.last != right(j)) {
          res = res :+ right(j)
        }
        j += 1
      } else {
        if (res.isEmpty || res.last != left(i)) {
          res = res :+ left(i)
        }
        i += 1
        j += 1
      }
    }

    while (i < left.length) {
      if (res.isEmpty || res.last != left(i)) {
        res = res :+ left(i)
      }
      i += 1
    }

    while (j < right.length) {
      if (res.isEmpty || res.last != right(j)) {
        res = res :+ right(j)
      }
      j += 1
    }

    res
  }

  def main(args: Array[String]): Unit = {
    val v = Vector(5, 1, 4, 2, 8, 3, 3, 2, 1)
    val sortedUnique = mergeSort(v)
    println(sortedUnique)
  }
}


