package view

import d.negyedik.het.AknakeresőGeneráló.{Akna, Szám, Tábla}

object AknakeresőKonzolon {

  def írdKiEgymásMellé(táblák: Tábla*): Unit = {

    def formázd(tábla: Tábla): List[List[String]] = {
      for (sor <- tábla) yield
        for (cella <- sor) yield cella match {
          case Akna => "*"
          case Szám(n) => n.toString
        }
    }

    val elválasztó = "   "

    println(
      táblák.map(formázd)
        .transpose
        .map(_.map(_.mkString).mkString(elválasztó)).mkString("\n")
    )
  }

}
