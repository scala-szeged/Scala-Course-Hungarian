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
      a, b, c, c, a
    )
    println

    val jólElkezdettA = takardKi(0, 2, takardBeMind(a))
    val jólElkezdettC = takardKi(0, 0, takardKi(2, 0, takardBeMind(c)))
    view.AknakeresőKonzolon.írdKiEgymásMellé(
      takardKi(3, 0, takardBeMind(a)),
      takardKi(3, 0, takardBeMind(b)),
      takardKi(3, 0, takardBeMind(c)),
      jólElkezdettC,
      jólElkezdettA
    )

    println
    view.AknakeresőKonzolon.írdKiEgymásMellé(
      a :: oldMegLépésenként(jólElkezdettA)
    )

    println
    view.AknakeresőKonzolon.írdKiEgymásMellé(
      c :: oldMegLépésenként(jólElkezdettC)
    )
  }

  def oldMegLépésenként(kiindulóTábla: Tábla): List[Tábla] = {
    lépj(List(kiindulóTábla)).reverse
  }

  def lépj(táblák: List[Tábla]): List[Tábla] = {
    val aknák = keresdAzAknákat(táblák.head)
    val tk2 = aknák.foldLeft(táblák) {
      case (tábla :: előzőTáblák, (aknaX, aknaY)) =>
        ténylegAknaE(aknaX, aknaY, tábla)
        takardKi(aknaX, aknaY, tábla) :: tábla :: előzőTáblák
    }
    aknák.foldLeft(tk2) {
      case (tk3, (aknaX, aknaY)) =>
        val tk4 = takardKiANemAknákat(aknaX, aknaY, tk3)
        if (vanMégTakartCella(tk4.head)) {
          lépj(tk4)
        } else {
          tk4
        }
    }
  }

  def keresdAzAknákat(tábla: Tábla): List[(Int, Int)] = {
    cellák(Nil: List[(Int, Int)], tábla) {
      case (lista, Szám(n), cx, cy) if n > 0 =>
        val tszk = takartSzomszédok(cx, cy, tábla)
        if (n == tszk.size)
          tszk ::: lista
        else
          lista

      case (lista, _, _, _) =>
        lista
    }
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

  def szomszédok[A <: List[Tábla]](a: A, cx: Int, cy: Int)(f: (A, Cella, Int, Int) => A): A = {
    val táblaAzElején = a.head
    val startX = max(0, cx - 1)
    val endX = min(táblaAzElején.head.size - 1, cx + 1)

    val endY = min(táblaAzElején.size - 1, cy + 1)
    val startY = max(0, cy - 1)

    (startX to endX).foldLeft(a) { (átmenetiA, x) =>
      (startY to endY).foldLeft(átmenetiA) { (igaziA, y) =>
        if (x != cx || y != cy) {
          val tábla = igaziA.head
          f(igaziA, tábla(y)(x), x, y)
        } else {
          igaziA
        }
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

  def vanMégTakartCella(tábla: Tábla): Boolean = {
    val tck = cellák(0, tábla) {
      case (n, _: TakartCella, _, _) => n + 1
      case (n, _, _, _) => n
    }
    tck > 0
  }

  def látszódóSzomszédAknákSzáma(cx: Int, cy: Int, tábla: Tábla): Int = {
    szomszédok(0, cx, cy, tábla) {
      case (n, Akna, _, _) => n + 1
      case (n, _, _, _) => n
    }
  }

  def takardKiANemAknákat(aknaX: Int, aknaY: Int, táblák: List[Tábla]): List[Tábla] = {
    szomszédok(táblák, aknaX, aknaY) {
      case (tk, Szám(n), x, y) =>
        if (n == látszódóSzomszédAknákSzáma(x, y, tk.head))
          takardKiASzomszédokat(x, y, tk)
        else
          tk

      case (tk, _, _, _) =>
        tk
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

  def takardKiASzomszédokat(kiX: Int, kiY: Int, táblák: List[Tábla]): List[Tábla] = {

    def takardKiASzomszédokat(kiX: Int, kiY: Int): List[(Int, Int)] = {
      val tábla = táblák.head
      val startX = max(0, kiX - 1)
      val endX = min(tábla.head.size - 1, kiX + 1)

      val endY = min(tábla.size - 1, kiY + 1)
      val startY = max(0, kiY - 1)

      (startX to endX).foldLeft(Nil: List[(Int, Int)]) { (listaX, x) =>
        (startY to endY).foldLeft(listaX) { (listaY, y) =>
          if (x != kiX || y != kiY)
            takardKiACellát(x, y, listaY)
          else
            listaY
        }
      }
    }

    def takardKiACellát(kiX: Int, kiY: Int, lista: List[(Int, Int)]): List[(Int, Int)] = {
      táblák.head(kiY)(kiX) match {
        case _: TakartCella => Tuple2(kiX, kiY) :: lista
        case _ => lista
      }
    }

    def nullátTakartKi(x: Int, y: Int, tábla: Tábla): Boolean = tábla(y)(x) match {
      case TakartSzám(0) => true
      case _ => false
    }


    val maszk: Set[(Int, Int)] = táblábólMaszk(táblák.head)
    val lista = takardKiASzomszédokat(kiX, kiY)

    val (újTáblák, _) = lista.foldLeft((táblák, maszk)) { case ((tk, mszk), koordináta) =>
      val újMaszk = mszk - koordináta
      if (nullátTakartKi(koordináta._1, koordináta._2, tk.head)) {
        val újTábla = takardKi(koordináta._1, koordináta._2, tk.head)
        (újTábla :: tk, újMaszk)
      } else {
        val újTábla = maszkold(tk.head, újMaszk)
        (újTábla :: tk, újMaszk)
      }
    }
    újTáblák
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
