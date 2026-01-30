object Homework1 {

  // Задание 1
  def MySum(a: List[Int]): Int = {
    a.foldLeft(0) { (acc, x) =>
      acc + x
    }
  }

  // Задание 2
  def MyRange(n: Int, m: Int): List[Int] = {
    n match {
      case _ if n > m => Nil
      case _ if n == m => List(n)
      case _ => n :: MyRange(n + 1, m)
    }
  }

  def main(args: Array[String]): Unit = {
    println(MySum(List(1, 2, 3, 4)))
    println(MySum(List(-2, 5, 0)))

    println(MyRange(1, 5))
    println(MyRange(3, 3))
    println(MyRange(5, 1))
  }
}

