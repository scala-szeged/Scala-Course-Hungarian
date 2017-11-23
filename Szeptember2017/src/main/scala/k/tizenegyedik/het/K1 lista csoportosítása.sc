
def csoportosítsd(lista: List[Int]): List[List[Int]] = {

  @annotation.tailrec
  def loop(lista: List[Int], csoport: List[Int], csoportok: List[List[Int]]): List[List[Int]] =
    lista match {

      case Nil =>
        csoport :: csoportok

      case b :: bk =>
        csoport match {
          case Nil =>
            loop(bk, b :: csoport, csoportok)
          case a :: _ if b == a + 1 =>
            loop(bk, b :: csoport, csoportok)
          case _ =>
            loop(bk, b :: Nil, csoport :: csoportok)
        }
    }

  val csoportok = loop(lista, Nil, Nil)
  csoportok.reverse.map(csoport => csoport.reverse)
}
val csoportok25 = csoportosítsd(List(1, 2, 3, 10, 12, 20, 21))


def csoportosítsdÁlt[T](lista: List[T])(f: (T, T) => Boolean): List[List[T]] = {

  @annotation.tailrec
  def loop(lista: List[T], csoport: List[T], csoportok: List[List[T]]): List[List[T]] =
    lista match {

      case Nil =>
        csoport :: csoportok

      case b :: bk =>
        csoport match {
          case Nil =>
            loop(bk, b :: csoport, csoportok)
          case a :: _ if f(a, b) =>
            loop(bk, b :: csoport, csoportok)
          case _ =>
            loop(bk, b :: Nil, csoport :: csoportok)
        }
    }

  val csoportok = loop(lista, Nil, Nil)
  csoportok.reverse.map(csoport => csoport.reverse)
}

def g(a: Char, b: Char) = a == b

val csoportok54 = csoportosítsdÁlt("akkumulátor".toList)(g)

val csoportok56 = csoportosítsdÁlt("akkumulátor".toList) {
  case (a, b) => a == b
}

def f(a: Int, b: Int) = b == a + 2
val csoportok61 = csoportosítsdÁlt(List(1, 2, 3, 10, 12, 20, 21))(f)
