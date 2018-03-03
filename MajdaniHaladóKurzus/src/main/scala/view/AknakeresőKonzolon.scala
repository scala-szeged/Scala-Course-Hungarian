package view

import model.AknakeresoModel._


object AknakeresőKonzolon {

  def írdKiEgymásMellé(táblák: Tábla*): Unit = {
    írdKiEgymásMellé(táblák.toList)
  }

  def írdKiEgymásMellé(táblák: List[Tábla]): Unit = {
    val elválasztó = "   "

    println(
      táblák.map((t: Tábla) => cellánkéntString(t))
        .transpose.map(_.map(_.mkString).mkString(elválasztó)).mkString("\n")
    )

    /*
    val elválasztó = "   "

    val aTábla = List(List(1, 2), List(3, 4))
    // 12
    // 34

    val bTábla = List(List(6, 7), List(8, 9))
    // 67
    // 89

    val táblák = List(aTábla, bTábla)
    táblák: List[List[List[Int]]] = List(List(List(1, 2), List(3, 4)), List(List(6, 7), List(8, 9)))

    táblák.transpose
    res8: List[List[List[Int]]] = List(List(List(1, 2), List(6, 7)), List(List(3, 4), List(8, 9)))

    táblák.transpose.mkString("\n")
    res9: String =
    List(List(1, 2), List(6, 7))
    List(List(3, 4), List(8, 9))

    táblák.transpose.map(_.mkString(elválasztó)).mkString("\n")
    res10: String =
    List(1, 2)   List(6, 7)
    List(3, 4)   List(8, 9)

    táblák.transpose.map(_.map(_.mkString).mkString(elválasztó)).mkString("\n")
    res11: String =
    12   67
    34   89

     */
  }

  def írdKi(tábla: Tábla): Unit = println(cellánkéntString(tábla).map(_.mkString).mkString("\n"))

  def cellánkéntString(tábla: Tábla): List[List[String]] = {
    for (sor <- tábla) yield
      for (cella <- sor) yield cella match {
        case Akna => "*"
        case Szám(n) => n.toString
        case TakartAkna => "."
        case TakartSzám(_) => "."
      }
  }

}
