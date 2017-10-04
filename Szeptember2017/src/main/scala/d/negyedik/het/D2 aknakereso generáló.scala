package d.negyedik.het

import java.lang.Math.abs


object AknakeresőGeneráló {

  class Cella

  case object Akna extends Cella

  case class Szám(n: Int) extends Cella

  case object TakartAkna extends Cella

  case class TakartSzám(n: Int) extends Cella

  type Tábla = List[List[Cella]]


  def main(args: Array[String]): Unit = {

    val üres = List(
      List(Szám(0), Szám(0), Szám(0), Szám(0), Szám(0)),
      List(Szám(0), Szám(0), Szám(0), Szám(0), Szám(0)),
      List(Szám(0), Szám(0), Szám(0), Szám(0), Szám(0))
    )

    val a = rakd(0, 1, rakd(0, 0, üres))
    val b = rakd(0, 1, üres)
    val c = rakd(1, 1, üres)
    val harom = List(a, b, c)

    view.AknakeresőKonzolon.írdKiEgymásMellé(
      harom
    )

    println

    view.AknakeresőKonzolon.írdKiEgymásMellé(
      takardKi(2, 0, takardLeMind(a)),
      takardKi(2, 0, takardLeMind(b)),
      takardKi(2, 0, takardLeMind(c))
    )
  }

  def rakd(x: Int, y: Int, tábla: Tábla): Tábla =
    for (sorÉsIndex <- tábla.zipWithIndex) yield
      for (cellaÉsIndex <- sorÉsIndex._1.zipWithIndex) yield
        (cellaÉsIndex._1, cellaÉsIndex._2, sorÉsIndex._2) match {

          case (_, cx, cy) if cx == x && cy == y =>
            Akna

          case (Szám(n), cx, cy) if abs(cx - x) <= 1 && abs(cy - y) <= 1 =>
            Szám(n + 1)

          case (c, _, _) =>
            c
        }


  def takardLeMind(tábla: Tábla): Tábla =
    for (sor <- tábla) yield
      for (cella <- sor) yield cella match {
        case Akna => TakartAkna
        case Szám(n) => TakartSzám(n)
      }

  def takardKi(x: Int, y: Int, tábla: Tábla): Tábla = {
    val ki = for (sorÉsIndex <- tábla.zipWithIndex) yield
      for (cellaÉsIndex <- sorÉsIndex._1.zipWithIndex) yield {
        val cella = cellaÉsIndex._1
        val cellaIndex = cellaÉsIndex._2
        val sorIndex = sorÉsIndex._2
        (tábla(sorIndex)(cellaIndex), cella, cellaIndex, sorIndex) match {

          case (_, TakartAkna, cx, cy) if cx == x && cy == y =>
            Akna

          case (Szám(0), TakartSzám(n), cx, cy) if abs(cx - x) <= 1 && abs(cy - y) <= 1 =>
            Szám(n)

          case (_, TakartSzám(n), cx, cy) if cx == x && cy == y =>
            Szám(n)

          case (_, c, _, _) =>
            c
        }
      }

    if(tábla(y)(x)==TakartSzám(0) && ki(y)(x)==Szám(0))
      takardKi(x,y,ki)
    else
      ki
  }
}
