
// Feladat: Mi okozza a hibás eredményt?
def maximum(lista: List[Int]): Int = lista match {

  case x :: többi =>
    val y = maximum(többi)
    if (y > x)
      y
    else
      x

  case x :: List() =>
    x

  case List() =>
    throw new Exception("Üres listára nem számítható maximum")
}

maximum(List(3, 2, 7))
