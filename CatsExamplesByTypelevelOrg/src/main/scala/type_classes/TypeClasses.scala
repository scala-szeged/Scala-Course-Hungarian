package type_classes

/**
  * https://typelevel.org/cats/typeclasses.html
  */
object TypeClasses extends App {

  trait Monoid[A] {
    def empty: A

    def combine(x: A, y: A): A
  }

  object Monoid {
    def apply[A: Monoid]: Monoid[A] = implicitly[Monoid[A]]
  }

  def combineAll[A: Monoid](list: List[A]): A =
    list.foldRight(Monoid[A].empty)(Monoid[A].combine)

  implicit val intAdditionMonoid: Monoid[Int] = new Monoid[Int] {
    def empty = 0

    def combine(x: Int, y: Int): Int = x + y
  }

  implicit val stringConcatMonoid: Monoid[String] = new Monoid[String] {
    def empty = ""

    def combine(x: String, y: String): String = x ++ y
  }

  implicit def unionMonoid[A]: Monoid[Set[A]] = new Monoid[Set[A]] {
    def empty = Set.empty[A]

    def combine(x: Set[A], y: Set[A]): Set[A] = x union y
  }

  println(combineAll(List(1, 2, 3, 4)))
  println(combineAll(List("Hello", " ", "World", "!")))
  println(combineAll(List(Set('a', 'b'), Set('c'))))
  println

  final case class Pair[A, B](first: A, second: B)

  object Pair {
    implicit def tuple2Instance[A, B](implicit A: Monoid[A], B: Monoid[B]): Monoid[Pair[A, B]] =
      new Monoid[Pair[A, B]] {
        def empty: Pair[A, B] = Pair(A.empty, B.empty)

        def combine(x: Pair[A, B], y: Pair[A, B]): Pair[A, B] =
          Pair(A.combine(x.first, y.first), B.combine(x.second, y.second))
      }
  }

  println(combineAll(Pair(1, "hello") :: Pair(2, " ") :: Pair(3, "world") :: Nil))
}