package k.tizenegyedik.het

import model.AknakeresoModel._

import scala.collection.immutable.ListSet

object K2AknakeresoFunkcionálisLego {

  def main(args: Array[String]): Unit = {

    val üres = List.tabulate(3, 5) { (_, _) => Szám(0) }

    val a = rakjAknát(üres, (0, 0), (1, 0), (4, 2))
    val b = rakjAknát(üres, 0, 1)
    val c = rakjAknát(üres, 2, 1)
    val d = rakjAknát(üres, (0, 0), (1, 0), (1, 1), (3, 1), (0, 2), (4, 2))

    view.AknakeresőKonzolon.írdKiEgymásMellé(
      a, b, c, c, a, d
    )
    println

    val jólElkezdettA = takardKi(0, 2, takardBeMind(a))
    val jólElkezdettB = takardKi(0, 0, takardKi(1, 0, takardKi(1, 1, takardBeMind(b))))
    val jólElkezdettC = takardKi(0, 0, takardKi(2, 0, takardBeMind(c)))
    val jólElkezdettD = takardKi(3, 0, takardKi(4, 0, takardKi(4, 1, takardBeMind(d))))
    view.AknakeresőKonzolon.írdKiEgymásMellé(
      takardKi(3, 0, takardBeMind(a)),
      takardKi(3, 0, takardBeMind(b)),
      takardKi(3, 0, takardBeMind(c)),
      jólElkezdettC,
      jólElkezdettA,
      jólElkezdettD
    )
    println

    view.AknakeresőKonzolon.írdKiEgymásMellé(
      b :: oldMegLépésenként(jólElkezdettB)
    )
    println

    view.AknakeresőKonzolon.írdKiEgymásMellé(
      c :: oldMegLépésenként(jólElkezdettC)
    )
    println

    view.AknakeresőKonzolon.írdKiEgymásMellé(
      a :: oldMegLépésenként(jólElkezdettA)
    )
    println

    view.AknakeresőKonzolon.írdKiEgymásMellé(
      d :: oldMegLépésenként(jólElkezdettD)
    )
  }

  def oldMegLépésenként(kiindulóTábla: Tábla): Táblák = {
    lépj(List(kiindulóTábla)).reverse
  }

  def lépj(régi: Táblák): Táblák = {
    val lépés = jelöldAzAknákat andThen takardKiANemAknákat
    val újTábla :: régiTáblák = lépés(régi)
    if (újTábla != régi.head)
      lépj(újTábla :: régiTáblák)
    else
      újTábla :: régiTáblák
  }

  val jelöldAzAknákat: Táblák => Táblák = { táblák =>
    keresdAzAknákat(táblák.head).foldLeft(táblák) {
      case (tábla :: előzőTáblák, (aknaX, aknaY)) =>
        jelezzHibátHaNemTakartAkna(aknaX, aknaY, tábla)
        takardKi(aknaX, aknaY, tábla) :: tábla :: előzőTáblák
    }
  }

  val takardKiANemAknákat: Táblák => Táblák = { táblák =>
    implicit val tábla: Tábla = táblák.head
    val takartSzomszédjaiNemAknák = cellák filter összesAknájaLátszódik filter vanTakartSzomszédja
    takartSzomszédjaiNemAknák.foldLeft(táblák)(takardKiASzomszédokat)
  }

  def keresdAzAknákat(implicit tábla: Tábla): KoordinátaLista = (cellák flatMap aknák).distinct

  def aknák(implicit tábla: Tábla): ((Int, Int)) => Set[(Int, Int)] = {
    implicit koordináták =>

      if ((nemNullaSzám getOrElse -1) == (szomszédok count takart) + (szomszédok count látszódóAkna))
        szomszédok filter takart
      else
        Set()
  }

  def jelezzHibátHaNemTakartAkna(aknaX: Int, aknaY: Int, tábla: Tábla): Unit = {
    tábla(aknaY)(aknaX) match {
      case TakartAkna => // OK
      case _ => println(aknaX, aknaY, " nem takart akna, pedig úgy számoltuk, hogy az")
        println("ebben a sorban volt a nem takart akna ", tábla(aknaY))
    }
  }

  def cellák(implicit tábla: Tábla): List[(Int, Int)] = for {
    x <- tábla.head.indices.toList
    y <- tábla.indices
  } yield (x, y)

  def szomszédok(implicit koordináták: (Int, Int), tábla: Tábla): Set[(Int, Int)] = {
    val (x, y) = koordináták
    val szk = ListSet(
      (x - 1, y - 1), (x, y - 1), (x + 1, y - 1),
      (x - 1, y), /*           */ (x + 1, y),
      (x - 1, y + 1), (x, y + 1), (x + 1, y + 1)
    )
    szk filter rajtaVanATáblán
  }

  def rajtaVanATáblán(implicit tábla: Tábla): ((Int, Int)) => Boolean = {
    case (x, y) => (tábla.head.indices contains x) && (tábla.indices contains y)
  }

  def nemNullaSzám(implicit koordináták: ((Int, Int)), tábla: Tábla): Option[Int] =
    koordináták match {
      case ((x, y)) => tábla(y)(x) match {
        case Szám(n) if n > 0 => Some(n)
        case _ => None
      }
    }

  def takart(implicit tábla: Tábla): ((Int, Int)) => Boolean = {
    case ((x, y)) => tábla(y)(x).isInstanceOf[TakartCella]
  }

  def látszódóAkna(implicit tábla: Tábla): ((Int, Int)) => Boolean = {
    case (x, y) => tábla(y)(x) == Akna
  }

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

  def vanTakartSzomszédja(implicit tábla: Tábla): ((Int, Int)) => Boolean = {
    implicit koordináták => szomszédok exists takart
  }

  //noinspection TypeAnnotation
  def takardKiASzomszédokat(táblák: Táblák, koordináták: (Int, Int)): Táblák = {
    implicit val t = táblák.head
    implicit val kk = koordináták
    val takartSzomszédok = szomszédok filter takart
    val takartNullaSzomszéd: Option[(Int, Int)] = takartSzomszédok find takartNullaSzám
    if (takartNullaSzomszéd.nonEmpty)
      takartNullaSzomszéd.foldLeft(táblák) {
        case (tk, (x, y)) => takardKi(x, y, tk.head) :: tk
      }
    else
      takartSzomszédok.foldLeft(táblák) {
        case (tk, (x, y)) => takardKi(x, y, tk.head) :: tk
      }
  }

  def takardKi(kiX: Int, kiY: Int, tábla: Tábla): Tábla = {

    def takarjKiCsakEgyet(tábla: Tábla, c: (Int, Int)): Tábla = {
      val (x, y) = c
      tábla(y)(x) match {
        case TakartSzám(n) =>
          módosítottTábla(tábla, x, y, Szám(n))
        case TakartAkna =>
          módosítottTábla(tábla, x, y, Akna)
      }
    }

    tábla(kiY)(kiX) match {
      case TakartSzám(0) =>
        nullaSzomszédokSzomszédjai((kiX, kiY), tábla).foldLeft(tábla)(takarjKiCsakEgyet)
      case _ =>
        takarjKiCsakEgyet(tábla, (kiX, kiY))
    }
  }

  def nullaSzomszédokSzomszédjai(nulla: (Int, Int), tábla: Tábla): Set[(Int, Int)] = {

    // todo miért igaz módosítás nélkül is, hogy aknákat nem takar ki?
    //noinspection TypeAnnotation
    def loop(szomszédokEddig: Set[(Int, Int)], c: (Int, Int)): Set[(Int, Int)] = {
      implicit val cella = c
      implicit val t = tábla
      val (x, y) = c

      tábla(y)(x) match {
        case TakartSzám(0) =>
          (szomszédok + c -- szomszédokEddig).foldLeft(szomszédok + c ++ szomszédokEddig)(loop)
        case TakartSzám(_) =>
          (szomszédokEddig + c).filter(takart)
        case _ =>
          szomszédokEddig.filter(takart)
      }
    }

    loop(Set(), nulla)
  }

  def takartNullaSzám(implicit tábla: Tábla): ((Int, Int)) => Boolean = {
    case ((x, y)) => tábla(y)(x) match {
      case TakartSzám(0) => true
      case _ => false
    }
  }

  def módosítottTábla(tábla: Tábla, x: Int, y: Int, cella: Cella): Tábla = tábla.updated(y, tábla(y).updated(x, cella))

  def rakjAknát(tábla: Tábla, hova: (Int, Int)*): Tábla = hova.foldLeft(tábla) { case (t, (x, y)) => rakjAknát(t, x, y) }

  def rakjAknát(tábla: Tábla, rakdX: Int, rakdY: Int): Tábla =
    List.tabulate(tábla.size, tábla.head.size) {
      case (`rakdY`, `rakdX`) =>
        Akna
      case (y, x) if szomszédok((rakdX, rakdY), tábla).contains(x, y) =>
        tábla(y)(x) match {
          case Szám(n) => Szám(n + 1)
          case _ => tábla(y)(x)
        }
      case (y, x) => tábla(y)(x)
    }

  def takardBeMind(tábla: Tábla): Tábla =
    for (sor <- tábla) yield
      for (cella <- sor) yield cella match {
        case Akna => TakartAkna
        case Szám(n) => TakartSzám(n)
      }
}
