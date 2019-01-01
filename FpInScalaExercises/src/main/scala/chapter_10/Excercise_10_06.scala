package chapter_10

object Excercise_10_06 {

  trait Monoid[A] {
    def op(a1: A, a2: A): A

    def zero: A
  }

  def foldMap[A, B](xs: List[A], m: Monoid[B])(f: A => B): B =
    xs.foldRight(m.zero)((a, b) => m.op(f(a), b))

  def foldMap2[A, B](xs: List[A], m: Monoid[B])(f: A => B): B =
    xs.foldLeft(m.zero)((b, a) => m.op(b, f(a)))

  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B = {
    class Mid extends Monoid[B => B] {
      override def op(b1: B => B, b2: B => B): B => B = b1 compose b2

      override def zero: B => B = b => b
    }
    val m = new Mid
    foldMap(as, m /*endoMonoid[B]*/)(f.curried)(z)
    //as.foldRight(m.zero)((a, b) => m.op(f(a), b))
  }
}
