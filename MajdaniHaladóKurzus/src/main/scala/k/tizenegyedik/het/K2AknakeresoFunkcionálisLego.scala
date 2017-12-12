package k.tizenegyedik.het

import java.lang.Math.{abs, max, min}

import model.AknakeresoModel._

/*
Példát csináltam rá:
foldRight - ezek a foldLeft -ek lecserélhetőek foldRight -ra
map - for + yield lecserélhető rá
fill - üres tábla előállítása
tabulate - rakd


Ötletek:
colllect - szomszédok
concat - lépj átszervezése: ne Táblák, csak Tábla legyen, amiket visszaadnak, azokat concat -tal egybe fűzhetjük
exists - Tábla.cellák bevezetésekor a vanMégTakartCella átszervezhető exists -re
filter - szomszédok mint collection bevezetésekor takartSzomszédok átszervezhető filter -re
intersect - takartSzomszédok
isEmpty - vanMégTakartCella
partition - Tábla.cellák bevezetésekor a takart és a nem takart cellák mint collection -ök előállítására
range - startX to endX helyett
updated - több helyen használható, talán sehol sem célszerű


Már használatban volt:
foldLeft
head
mkString a kiíratáskor
reverse
zipWithIndex
 */
object K2AknakeresoFunkcionálisLego {

  def main(args: Array[String]): Unit = {

    val üres = List.tabulate(3, 5) { (_, _) => Szám(0) }

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

  def oldMegLépésenként(kiindulóTábla: Tábla): Táblák = {
    lépj(List(kiindulóTábla)).reverse
  }

  //noinspection ConvertibleToMethodValue
  def lépj(régi: Táblák): Táblák = {
    val lépés = jelöldAzAknákat _ andThen takardKiANemAknákat _
    val új = lépés(régi)
    if (új != régi)
      lépj(új)
    else
      régi
  }

  def jelöldAzAknákat(táblák: Táblák): Táblák = {
    val aknák = keresdAzAknákat(táblák.head)
    aknák.foldLeft(táblák) {
      case (tábla :: előzőTáblák, (aknaX, aknaY)) =>
        ténylegAknaE(aknaX, aknaY, tábla)
        takardKi(aknaX, aknaY, tábla) :: tábla :: előzőTáblák
    }
  }

  def takardKiANemAknákat(táblák: Táblák): Táblák = {
    implicit val tábla = táblák.head
    (cellák filter összesAknájaLátszódik).foldLeft(táblák)(takardKiASzomszédokat)
  }

  def keresdAzAknákat(implicit tábla: Tábla): KoordinátaLista =
    cellák flatMap { implicit koordináták =>

      if ((nemNullaSzám getOrElse -9) == (szomszédok count takart) + (szomszédok count látszódóAkna))
        szomszédok filter takart
      else
        List()
    }

  def nemNullaSzám(implicit koordináták: (Int, Int), tábla: Tábla): Option[Int] =
    koordináták match {
      case ((x, y)) => tábla(y)(x) match {
        case Szám(n) if n > 0 => Some(n)
        case _ => None
      }
    }

  def takart(implicit tábla: Tábla): ((Int, Int)) => Boolean = {
    case ((x, y)) =>
      tábla(y)(x) match {
        case _: TakartCella =>
          true

        case _ =>
          false
      }
  }

  def cellák(implicit tábla: Tábla): List[(Int, Int)] = for {
    x <- tábla.head.indices.toList
    y <- tábla.indices
  } yield (x, y)

  //noinspection MatchToPartialFunction
  def összesAknájaLátszódik(implicit tábla: Tábla): ((Int, Int)) => Boolean = {
    implicit koordináták: (Int, Int) =>
      koordináták match {
        case ((x, y)) => tábla(y)(x) match {
          case Szám(0) =>
            false

          case Szám(n) =>
            n == (szomszédok count látszódóAkna)

          case _ =>
            false
        }
      }
  }

  def szomszédok(implicit koordináták: (Int, Int), tábla: Tábla): Set[(Int, Int)] = {
    val szk: Set[(Int, Int)] = koordináták match {
      case (x, y) => for (q <- Set(
        (x + 1, y - 1), (x + 1, y), (x + 1, y + 1),
        (x, y - 1), /*           */ (x, y + 1),
        (x - 1, y - 1), (x - 1, y), (x - 1, y + 1))) yield q
    }

    szk filter rajtaVanATáblán
  }

  def rajtaVanATáblán(implicit tábla: Tábla): ((Int, Int)) => Boolean = {
    case (x, y) => (tábla.head.indices contains x) && (tábla.indices contains y)
  }

  def látszódóAkna(implicit tábla: Tábla): ((Int, Int)) => Boolean = {
    case (x, y) => tábla(y)(x) == Akna
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

  def takardKiASzomszédokat(táblák: Táblák, koordináták: (Int, Int)): Táblák = {

    def takardKiASzomszédokat(kiX: Int, kiY: Int): KoordinátaLista = {
      val tábla = táblák.head
      val startX = max(0, kiX - 1)
      val endX = min(tábla.head.size - 1, kiX + 1)

      val endY = min(tábla.size - 1, kiY + 1)
      val startY = max(0, kiY - 1)

      (startX to endX).foldLeft(Nil: KoordinátaLista) { (listaX, x) =>
        (startY to endY).foldLeft(listaX) { (listaY, y) =>
          if (x != kiX || y != kiY)
            takardKiACellát(x, y, listaY)
          else
            listaY
        }
      }
    }

    def takardKiACellát(kiX: Int, kiY: Int, lista: KoordinátaLista): KoordinátaLista = {
      táblák.head(kiY)(kiX) match {
        case _: TakartCella => Tuple2(kiX, kiY) :: lista
        case _ => lista
      }
    }

    def nullátTakartKi(x: Int, y: Int, tábla: Tábla): Boolean = tábla(y)(x) match {
      case TakartSzám(0) => true
      case _ => false
    }


    val maszk: KoordinátaSet = táblábólMaszk(táblák.head)
    val (kiX, kiY) = koordináták
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

  def takardKi(kiX: Int, kiY: Int, tábla: Tábla): Tábla = {

    def takardKiACellát(kiX: Int, kiY: Int, maszk: KoordinátaSet): KoordinátaSet = {
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

    def takardKiASzomszédokat(kiX: Int, kiY: Int, maszk: KoordinátaSet): KoordinátaSet = {
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

    val maszk: KoordinátaSet = táblábólMaszk(tábla)

    maszkold(tábla, takardKiACellát(kiX, kiY, maszk))
  }

  def táblábólMaszk(tábla: Tábla): KoordinátaSet = {
    val maszkLista =
      for {
        (sor, sorIndex) <- tábla.zipWithIndex
        (cella, cellaIndex) <- sor.zipWithIndex
        if cella == TakartAkna || cella.isInstanceOf[TakartSzám]
      }
        yield (cellaIndex, sorIndex)

    maszkLista.toSet
  }

  def maszkold(tábla: Tábla, maszk: KoordinátaSet): Tábla = {
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

  def rakd(rakdX: Int, rakdY: Int, tábla: Tábla): Tábla =
    List.tabulate(tábla.size, tábla.head.size) {
      case (`rakdY`, `rakdX`) =>
        Akna
      case (y, x) if abs(x - rakdX) <= 1 && abs(y - rakdY) <= 1 =>
        tábla(y)(x) match {
          case Szám(n) => Szám(n + 1)
          case _ => tábla(y)(x)
        }
      case (y, x) => tábla(y)(x)
    }
}
