object TopNSort {

  def topN(v: Vector[Int], n: Int): Vector[Int] = {
    if (n <= 0) {
      Vector()
    } else {
      var res = Vector[Int]()

      for (x <- v) {
        res = res :+ x
        res = res.sorted

        if (res.length > n) {
          res = res.take(n)
        }
      }

      res
    }
  }

  def main(args: Array[String]): Unit = {
    val v = Vector(5, 1, 4, 2, 8, 3, 3)
    val n = 3

    val result = topN(v, n)
    println(result)
  }
}

