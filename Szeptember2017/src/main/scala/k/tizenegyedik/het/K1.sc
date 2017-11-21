
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
