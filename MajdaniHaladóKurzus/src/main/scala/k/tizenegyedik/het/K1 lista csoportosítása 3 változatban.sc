
def csoportosítsd7(lista: List[Int]): List[List[Int]] = {

  @annotation.tailrec
  def loop(lista: List[Int], csoportok: List[List[Int]]): List[List[Int]] =
    csoportok match {

      case Nil => lista match {
        case Nil =>
          csoportok
        case b :: bk =>
          loop(bk, (b :: Nil) :: csoportok)
      }

      case (a :: ak) :: csk => lista match {
        case Nil =>
          csoportok
        case b :: bk if b == a + 1 =>
          loop(bk, (b :: a :: ak) :: csk)
        case b :: bk =>
          loop(bk, (b :: Nil) :: csoportok)
      }
    }

  val csoportok = loop(lista, Nil)
  csoportok.reverse.map(csoport => csoport.reverse)
}
val csoportok7 = csoportosítsd7(List(1, 2, 3, 10, 12, 20, 21))


def csoportosítsd5(lista: List[Int]): List[List[Int]] = {

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
val csoportok5 = csoportosítsd5(List(1, 2, 3, 10, 12, 20, 21))


case class <--->[X, Y](x: X, y: Y)

implicit class KisebbKötőjelekNagyobb[X, Y](x: X) {
  def <--->(y: Y) = new <--->(x, y)
}

def csoportosítsd3[T](lista: List[T])(feltétel: (T, T) => Boolean): List[List[T]] = {

  @annotation.tailrec
  def loop(lista: List[T], csoportok: List[List[T]]): List[List[T]] =
    lista <---> csoportok match {
      case Nil <---> _ =>
        csoportok
      case b :: bk <---> (a :: ak) :: csk if feltétel(a, b) =>
        loop(bk, (b :: a :: ak) :: csk)
      case b :: bk <---> _ =>
        loop(bk, (b :: Nil) :: csoportok)
    }

  val csoportok = loop(lista, Nil)
  csoportok.reverse.map(csoport => csoport.reverse)
}

val csoportokPlusz1 = csoportosítsd3(List(1, 2, 3, 10, 12, 20, 21)) {
  (a, b) => b == a + 1
}


val csoportokPlusz2 = csoportosítsd3(List(1, 2, 3, 10, 12, 20, 21)) {
  (a, b) => b == a + 2
}
