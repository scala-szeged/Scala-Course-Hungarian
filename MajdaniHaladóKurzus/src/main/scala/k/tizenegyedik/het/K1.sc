
@annotation.tailrec
def csoportosítsd(lista: List[Int], csoportok: List[List[Int]]): List[List[Int]] = csoportok match {
  case Nil => lista match {
    case Nil =>
      csoportok
    case b :: bk =>
      csoportosítsd(bk, (b :: Nil) :: csoportok)
  }

  case (a :: ak) :: csk => lista match {
    case Nil =>
      csoportok
    case b :: bk if b == a + 1 =>
      csoportosítsd(bk, (b :: a :: ak) :: csk)
    case b :: bk =>
      csoportosítsd(bk, (b :: Nil) :: csoportok)
  }
}

val csoportok = csoportosítsd(List(1, 2, 3, 10, 12, 20, 21), Nil)
csoportok.map(csoport => csoport.reverse).reverse


case class <--->[X, Y](x: X, y: Y)

implicit class KisebbKötőjelekNagyobb[X, Y](x: X) {
  def <--->(y: Y) = new <--->(x, y)
}

def csoportosítsdÁlt[T](lista: List[T])(feltétel: (T, T) => Boolean): List[List[T]] = {

  @annotation.tailrec
  def go(lista: List[T], csoportok: List[List[T]]): List[List[T]] =
    lista <---> csoportok match {
      case Nil <---> _ =>
        csoportok
      case b :: bk <---> (a :: ak) :: csk if feltétel(a, b) =>
        go(bk, (b :: a :: ak) :: csk)
      case b :: bk <---> _ =>
        go(bk, (b :: Nil) :: csoportok)
    }

  go(lista, Nil)
}

val csoportok2 = csoportosítsdÁlt(List(1, 2, 3, 10, 12, 20, 21)) {
  (a, b) => b == a + 1
}
csoportok2.map(csoport => csoport.reverse).reverse

val csoportok3 = csoportosítsdÁlt(List(1, 2, 3, 10, 12, 20, 21)) {
  (a, b) => b == a + 1
}
csoportok3.map(csoport => csoport.reverse).reverse
