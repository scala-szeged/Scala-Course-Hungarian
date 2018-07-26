package d.chapter_4

/**
  * „Implement versions of map, flatMap, orElse, and map2 on Either that operate on the Right value.”
  */
object Exercise_4_6 extends App {

  sealed trait Either[+E, +A] {
    def map[B](f: A => B): Either[E, B]

    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B]
  }

  case class Left[+E](value: E) extends Either[E, Nothing] {
    override def map[B](f: Nothing => B): Either[E, B] = this

    override def flatMap[EE >: E, B](f: Nothing => Either[EE, B]): Either[EE, B] = this
  }

  case class Right[+A](value: A) extends Either[Nothing, A] {
    override def map[B](f: A => B): Either[Nothing, B] = Right(f(value))

    override def flatMap[EE >: Nothing, B](f: A => Either[EE, B]): Either[EE, B] = f(value)
  }

  println(Left("Error happened"), Left("Error happened").map(_ => 1))
  println(Right(2), Right(2).map(_ * 3))
  println

  private def TwentyDividedBy(x: Int) =
    if (x == 0) Left("Division by zero") else Right(20 / x)

  println(Right(4), Right(4).flatMap(TwentyDividedBy))
  println(Right(0), Right(0).flatMap(TwentyDividedBy))
  println
}
