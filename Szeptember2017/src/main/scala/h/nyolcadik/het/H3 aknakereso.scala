package h.nyolcadik.het

import model.AknakeresoModel._
import java.lang.Math.abs


object H3_Aknakereső {

  def main(args: Array[String]): Unit = {

    val üres = List(
      List(Szám(0), Szám(0), Szám(0), Szám(0), Szám(0)),
      List(Szám(0), Szám(0), Szám(0), Szám(0), Szám(0)),
      List(Szám(0), Szám(0), Szám(0), Szám(0), Szám(0))
    )

    val a = rakd(0, 0, rakd(1, 0, rakd(4, 2, üres)))
    val b = rakd(0, 1, üres)
    val c = rakd(2, 1, üres)

    view.AknakeresőKonzolon.írdKiEgymásMellé(
      a, b, c
    )

    println

    view.AknakeresőKonzolon.írdKiEgymásMellé(
      takardKi(3, 0, takardBeMind(a)),
      takardKi(3, 0, takardBeMind(b)),
      takardKi(3, 0, takardBeMind(c))
    )
  }

  def rakd(rakdX: Int, rakdY: Int, tábla: Tábla): Tábla =
    for {(sor, sorIndex) <- tábla.zipWithIndex} yield
      for {(cella, cellaIndex) <- sor.zipWithIndex} yield
        (cella, cellaIndex, sorIndex) match {

          case (_, cx, cy) if cx == rakdX && cy == rakdY =>
            Akna

          case (Szám(n), cx, cy) if abs(cx - rakdX) <= 1 && abs(cy - rakdY) <= 1 =>
            Szám(n + 1)

          case (c, _, _) =>
            c
        }


  def takardBeMind(tábla: Tábla): Tábla =
    tábla

  def takardKi(kiX: Int, kiY: Int, tábla: Tábla): Tábla = {

    def takardKiACellát(kiX: Int, kiY: Int, maszk: Set[(Int, Int)]): Set[(Int, Int)] = {
      maszk
    }

    def takardKiASzomszédokat(kiX: Int, kiY: Int, maszk: Set[(Int, Int)]): Set[(Int, Int)] = {
      maszk
    }

    val maszk: Set[(Int, Int)] = táblábólMaszk(tábla)

    maszkold(tábla, takardKiACellát(kiX, kiY, maszk))
  }

  private def táblábólMaszk(tábla: Tábla): Set[(Int, Int)] = {
    Set.empty
  }

  def maszkold(tábla: Tábla, maszk: Set[(Int, Int)]): Tábla = {
    tábla
  }
}
