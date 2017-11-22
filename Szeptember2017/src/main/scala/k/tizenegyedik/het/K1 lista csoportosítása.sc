

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

val csoportok = csoportosítsd(List(1, 2, 3, 10, 12, 20, 21))
