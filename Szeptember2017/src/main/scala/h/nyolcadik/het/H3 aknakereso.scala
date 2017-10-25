package h.nyolcadik.het

import model.AknakeresoModel._
import java.lang.Math.{abs, max, min}


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
      takardBeMind(a),
      takardBeMind(b),
      takardBeMind(c)
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


  def takardBeMind(tábla: Tábla): Tábla = tábla

  // --- foldLeft, új ismeret

  // def foldLeft[A](a: A)(f0: (A, T) => A): A
  // A - ez a kezdő érték és az eredmény típusa
  // f0 - függvény (metódus), ami kettő paraméterű:
  // az 1. paraméter az aktuális eredmény, ami először a kezdőérték
  // a 2. paraméter a soronkövetkező elem
  // az általa kiszámolt eredmény a következő f0 híváskor az 1. paraméter lesz

  def szomszédok[A](a: A, cx: Int, cy: Int, tábla: Tábla)(f: (A, Cella, Int, Int) => A): A = {
    val startX = max(0, cx - 1)
    val endX = min(tábla.head.size - 1, cx + 1)

    val endY = min(tábla.size - 1, cy + 1)
    val startY = max(0, cy - 1)

    (startX to endX).foldLeft(a) { (átmenetiA, x) =>
      (startY to endY).foldLeft(átmenetiA) { (igaziA, y) =>
        f(igaziA, tábla(y)(x), x, y)
      }
    }
  }

  def takartSzomszédok(cx: Int, cy: Int, tábla: Tábla): List[(Int, Int)] = {
    szomszédok(Nil: List[(Int, Int)], cx, cy, tábla) {
      case (lista, TakartSzám(_), x, y) => (x, y) :: lista
      case (lista, TakartAkna, x, y) => (x, y) :: lista
      case (lista, _, _, _) => lista
    }
  }

  def kitakartAknaSzomszédok(cx: Int, cy: Int, tábla: Tábla): Int = 0
}
