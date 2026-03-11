import play.api.libs.json._
import play.api.libs.functional.syntax._

object HomeworkSolutions {

  // ===== Task 1 =====
  lazy val fibonacci: LazyList[BigInt] = {
    def loop(a: BigInt, b: BigInt): LazyList[BigInt] =
      a #:: loop(b, a + b)

    loop(0, 1)
  }

  val first10DivBy3: Vector[BigInt] =
    fibonacci.filter(_ % 3 == 0).take(10).toVector

  // ===== Task 2 =====
  case class User(id: Int, name: String)
  case class Order(id: Int, userId: Int, amount: Double)

  def findUser(id: Int): Option[User] =
    if (id == 1) Some(User(1, "Ilya")) else None

  def getActiveOrder(user: User): Option[Order] =
    if (user.id == 1) Some(Order(101, user.id, 1200.0)) else None

  def calculateDiscount(order: Order): Either[String, Double] =
    if (order.amount < 500) Left("Слишком маленькая сумма для скидки")
    else Right(order.amount * 0.1)

  def getUserDiscount(userId: Int): Either[String, Double] =
    for {
      user     <- findUser(userId).toRight("Пользователь не найден")
      order    <- getActiveOrder(user).toRight("Активный заказ не найден")
      discount <- calculateDiscount(order)
    } yield discount

  // ===== Task 3 =====
  trait Validator[T] {
    def validate(value: T): Boolean
  }

  object Validator {
    implicit val stringValidator: Validator[String] = (value: String) => value.nonEmpty
    implicit val intValidator: Validator[Int] = (value: Int) => value > 0
  }

  def check[T](value: T)(implicit v: Validator[T]): Unit =
    if (v.validate(value)) println("OK") else println("Error")

  implicit class ValidatorOps[T](value: T) {
    def isValid(implicit v: Validator[T]): Boolean = v.validate(value)
  }

  // ===== Task 4 =====
  case class Product(id: Long, name: String, price: Double, tags: List[String])

  object Product {
    implicit val productFormat: Format[Product] = (
      (__ \ "id").format[Long] and
      (__ \ "name").format[String] and
      (__ \ "price").format[Double] and
      (__ \ "tags").format[List[String]]
    )(Product.apply, unlift(Product.unapply))
  }
}
