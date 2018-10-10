
// A megoldás elemei

val l327 = List(3, 2, 7)
l327 match {
  case x :: többi => "talált: case x :: többi"
  case _ => "nem jó: case x :: többi"
}

val l3 = List(3)
l3 match {
  case x :: List() => "talált: case x :: List()"
  case _ => "nem jó: case x :: List()"
}

val üresLista = List()
üresLista match {
  case List() => "talált: case List()"
  case _ => "nem jó: case List()"
}


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
