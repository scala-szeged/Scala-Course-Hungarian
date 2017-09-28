package view

import d.negyedik.het.AknakeresőGeneráló.{Akna, Szám, Tábla}

object AknakeresőKonzolon {

  def írdKiEgymásMellé(táblák: Tábla*): Unit = {

    def cellánkéntString(tábla: Tábla): List[List[String]] = {
      for (sor <- tábla) yield
        for (cella <- sor) yield cella match {
          case Akna => "*"
          case Szám(n) => n.toString
        }
    }

    val elválasztó = "   "

    println(
      táblák.map(cellánkéntString)
        .transpose.map(_.map(_.mkString).mkString(elválasztó)).mkString("\n")
    )

    /*
    val elválasztó = "   "

    val as = List(List(1, 2), List(3, 4))
    // 12
    // 34

    val bs = List(List(6, 7), List(8, 9))
    // 67
    // 89

    val t = List(as,bs)
    t: List[List[List[Int]]] = List(List(List(1, 2), List(3, 4)), List(List(6, 7), List(8, 9)))

    t.transpose
    res8: List[List[List[Int]]] = List(List(List(1, 2), List(6, 7)), List(List(3, 4), List(8, 9)))

    t.transpose.mkString("\n")
    res9: String =
    List(List(1, 2), List(6, 7))
    List(List(3, 4), List(8, 9))

    t.transpose.map(_.mkString(elválasztó)).mkString("\n")
    res10: String =
    List(1, 2)   List(6, 7)
    List(3, 4)   List(8, 9)

    t.transpose.map(_.map(_.mkString).mkString(elválasztó)).mkString("\n")
    res11: String =
    12   67
    34   89

     */
  }

}
