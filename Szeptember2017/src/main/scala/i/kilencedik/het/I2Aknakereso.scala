package i.kilencedik.het

import java.lang.Math.{abs, max, min}

import model.AknakeresoModel._


object I2Aknakereso {

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
      a, b, c, c
    )
    println

    val betakartC = takardKi(0, 0, takardKi(2, 0, takardBeMind(c)))
    view.AknakeresőKonzolon.írdKiEgymásMellé(
      takardKi(3, 0, takardBeMind(a)),
      takardKi(3, 0, takardBeMind(b)),
      takardKi(3, 0, takardBeMind(c)),
      betakartC
    )
    println

    view.AknakeresőKonzolon.írdKiEgymásMellé(
      c :: oldMegLépésenként(betakartC)
    )
  }

  def rakd(rakdX: Int, rakdY: Int, tábla: Tábla): Tábla =
    újTábla(tábla) {

      case (_, cx, cy) if cx == rakdX && cy == rakdY =>
        Akna

      case (Szám(n), cx, cy) if abs(cx - rakdX) <= 1 && abs(cy - rakdY) <= 1 =>
        Szám(n + 1)

      case (c, _, _) =>
        c
    }

  def újTábla[A](tábla: Tábla)(f: (Cella, Int, Int) => Cella): Tábla =
    for {(sor, sorIndex) <- tábla.zipWithIndex} yield
      for {(cella, cellaIndex) <- sor.zipWithIndex} yield
        f(cella, cellaIndex, sorIndex)


  def szomszédok[A](a: A, cx: Int, cy: Int, tábla: Tábla)(f: (A, Cella, Int, Int) => A): A = {
    val startX = max(0, cx - 1)
    val endX = min(tábla.head.size - 1, cx + 1)

    val endY = min(tábla.size - 1, cy + 1)
    val startY = max(0, cy - 1)

    (startX to endX).foldLeft(a) { (átmenetiA, x) =>
      (startY to endY).foldLeft(átmenetiA) { (igaziA, y) =>
        if (x != cx || y != cy)
          f(igaziA, tábla(y)(x), x, y)
        else
          igaziA
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

  def cellák[A](a: A, tábla: Tábla)(f: (A, Cella, Int, Int) => A): A = {
    tábla.zipWithIndex.foldLeft(a) { case (átmenetiA, (sor, sorIndex)) =>
      sor.zipWithIndex.foldLeft(átmenetiA) { case (igaziA, (cella, cellaIndex)) =>
        f(igaziA, cella, cellaIndex, sorIndex)
      }
    }
  }

  def keresd(tábla: Tábla): Option[(Int, Int)] = {
    val ck = cellák(Nil: List[(Int, Int)], tábla) {
      case (lista, Szám(1), cx, cy) =>
        val tszk = takartSzomszédok(cx, cy, tábla)
        if (1 == tszk.size)
          tszk.head :: lista
        else
          lista

      case (lista, _, _, _) =>
        lista
    }
    ck.headOption
  }

  def oldMegLépésenként(kiindulóTábla: Tábla): List[Tábla] = {
    lépj(List(kiindulóTábla)).reverse
  }

  def vanMégTakartCella(tábla: Tábla): Boolean = {
    val tck = cellák(0, tábla) {
      case (n, _: TakartCella, _, _) => n + 1
      case (n, _, _, _) => n
    }
    tck > 0
  }

  def csökkentsdASzomszédait(aknaX: Int, aknaY: Int, tábla: Tábla): Tábla =
    újTábla(tábla) {

      case (Szám(n), cx, cy) if abs(cx - aknaX) <= 1 && abs(cy - aknaY) <= 1 =>
        Szám(max(0, n - 1))

      case (c, _, _) =>
        c
    }

  def látszódóSzomszédAknákSzáma(cx: Int, cy: Int, tábla: Tábla): Int = {
    szomszédok(0, cx, cy, tábla) {
      case (n, Akna, x, y) => n + 1
      case (n, _, _, _) => n
    }
  }

  def takardKiANemAknákat(aknaX: Int, aknaY: Int, tábla: Tábla): Tábla = {
    szomszédok(tábla, aknaX, aknaY, tábla) {
      case (t, Szám(n), x, y) =>
        if (n == látszódóSzomszédAknákSzáma(x, y, t))
          takardKiASzomszédokat(x, y, t)
        else
          t

      case (t, _, _, _) =>
        t
    }
  }

  def lépj(táblák: List[Tábla]): List[Tábla] = {
    val tábla = táblák.head

    keresd(tábla) match {

      case None =>
        táblák

      case Some((aknaX, aknaY)) =>
        ténylegAknaE(aknaX, aknaY, tábla)
        val t2 = takardKi(aknaX, aknaY, tábla)
        val t3 = takardKiANemAknákat(aknaX, aknaY, t2)
        if (vanMégTakartCella(t2)) {
          lépj(
            //csökkentsdASzomszédait(aknaX, aknaY, t3) :: táblák
            t3 :: táblák
          )
        } else {
          táblák
        }
    }
  }

  def ténylegAknaE(aknaX: Int, aknaY: Int, tábla: Tábla): Unit = {
    tábla(aknaY)(aknaX) match {
      case TakartAkna | Akna => // OK
      case _ => println(aknaX, aknaY, " nem akna, pedig úgy számoltuk, hogy az")
        println("ebben a sorban volt a nem akna ", tábla(aknaY))
    }
  }

  def takardBeMind(tábla: Tábla): Tábla =
    for (sor <- tábla) yield
      for (cella <- sor) yield cella match {
        case Akna => TakartAkna
        case Szám(n) => TakartSzám(n)
      }


  def takardKi(kiX: Int, kiY: Int, tábla: Tábla): Tábla = {

    def takardKiACellát(kiX: Int, kiY: Int, maszk: Set[(Int, Int)]): Set[(Int, Int)] = {
      val maszkoltTábla = maszkold(tábla, maszk)
      maszkoltTábla(kiY)(kiX) match {
        case TakartSzám(0) => takardKiASzomszédokat(
          kiX, kiY, maszk - Tuple2(kiX, kiY)
        )
        case TakartSzám(_) => maszk - Tuple2(kiX, kiY)
        case TakartAkna => maszk - Tuple2(kiX, kiY)
        case _ => maszk
      }
    }

    def takardKiASzomszédokat(kiX: Int, kiY: Int, maszk: Set[(Int, Int)]): Set[(Int, Int)] = {
      val startX = max(0, kiX - 1)
      val endX = min(tábla.head.size - 1, kiX + 1)

      val endY = min(tábla.size - 1, kiY + 1)
      val startY = max(0, kiY - 1)

      (startX to endX).foldLeft(maszk) { (mx, x) =>
        (startY to endY).foldLeft(mx) { (my, y) =>
          takardKiACellát(x, y, my)
        }
      }
    }

    val maszk: Set[(Int, Int)] = táblábólMaszk(tábla)

    maszkold(tábla, takardKiACellát(kiX, kiY, maszk))
  }

  def takardKiASzomszédokat(kiX: Int, kiY: Int, tábla: Tábla): Tábla = {

    def takardKiASzomszédokat(kiX: Int, kiY: Int, maszk: Set[(Int, Int)]): Set[(Int, Int)] = {
      val startX = max(0, kiX - 1)
      val endX = min(tábla.head.size - 1, kiX + 1)

      val endY = min(tábla.size - 1, kiY + 1)
      val startY = max(0, kiY - 1)

      (startX to endX).foldLeft(maszk) { (mx, x) =>
        (startY to endY).foldLeft(mx) { (my, y) =>
          takardKiACellát(x, y, my)
        }
      }
    }

    def takardKiACellát(kiX: Int, kiY: Int, maszk: Set[(Int, Int)]): Set[(Int, Int)] = {
      val maszkoltTábla = maszkold(tábla, maszk)

      maszkoltTábla(kiY)(kiX) match {
        case _: TakartCella => maszk - Tuple2(kiX, kiY)
        case _ => maszk
      }
    }

    val maszk: Set[(Int, Int)] = táblábólMaszk(tábla)

    maszkold(tábla, takardKiASzomszédokat(kiX, kiY, maszk))
  }

  def táblábólMaszk(tábla: Tábla): Set[(Int, Int)] = {
    val maszkLista =
      for {
        (sor, sorIndex) <- tábla.zipWithIndex
        (cella, cellaIndex) <- sor.zipWithIndex
        if cella == TakartAkna || cella.isInstanceOf[TakartSzám]
      }
        yield (cellaIndex, sorIndex)

    maszkLista.toSet
  }

  def maszkold(tábla: Tábla, maszk: Set[(Int, Int)]): Tábla = {
    for {(sor, sorIndex) <- tábla.zipWithIndex} yield
      for {(cella, cellaIndex) <- sor.zipWithIndex} yield
        cella match {
          case Akna | TakartAkna =>
            if (maszk(cellaIndex, sorIndex))
              TakartAkna
            else
              Akna

          case Szám(n) =>
            if (maszk(cellaIndex, sorIndex))
              TakartSzám(n)
            else
              Szám(n)

          case TakartSzám(n) =>
            if (maszk(cellaIndex, sorIndex))
              TakartSzám(n)
            else
              Szám(n)
        }
  }
}
