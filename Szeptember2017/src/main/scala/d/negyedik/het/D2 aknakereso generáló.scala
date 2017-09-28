package d.negyedik.het

import java.lang.Math.abs


object AknakeresőGeneráló {

  trait Cella

  case object Akna extends Cella

  case class Szám(n: Int) extends Cella

  type Tábla = List[List[Cella]]


  def main(args: Array[String]): Unit = {

    val üres = List(
      List(Szám(0), Szám(0), Szám(0)),
      List(Szám(0), Szám(0), Szám(0)),
      List(Szám(0), Szám(0), Szám(0))
    )

    view.AknakeresőKonzolon.írdKiEgymásMellé(

      rakd(0, 1, rakd(0, 0, üres))
      ,
      rakd(0, 1, üres)
      ,
      rakd(1, 1, üres)
    )


    def rakd(x: Int, y: Int, tábla: Tábla): Tábla =
      for (sorIndex <- tábla.zipWithIndex) yield
        for (cellaIndex <- sorIndex._1.zipWithIndex) yield
          (cellaIndex._1, sorIndex._2, cellaIndex._2) match {

            case (_, cx, cy) if cx == x && cy == y =>
              Akna

            case (Szám(n), cx, cy) if abs(cx - x) <= 1 && abs(cy - y) <= 1 =>
              Szám(n + 1)

            case (c, _, _) =>
              c
          }


  }
}
