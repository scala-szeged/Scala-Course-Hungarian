package k.tizenegyedik.het

import model.AknakeresoModel._

import scala.collection.immutable.ListSet

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

  //noinspection ZeroIndexToHead
  def main(args: Array[String]): Unit = {

    val üres = List.tabulate(3, 5) { (_, _) => Szám(0) }

    val a = rakd(0, 0, rakd(1, 0, rakd(4, 2, üres)))
    val b = rakd(0, 1, üres)
    val c = rakd(2, 1, üres)
    val cVégjáték = c.
      updated(0, c(0).updated(4, TakartSzám(0))).
      updated(1, c(1).updated(4, TakartSzám(0))).
      updated(2, c(2).updated(4, TakartSzám(0)))

    view.AknakeresőKonzolon.írdKiEgymásMellé(
      a, b, c, c, a, cVégjáték
    )
    println

    val jólElkezdettA = takardKi(0, 2, takardBeMind(a))
    val jólElkezdettB = takardKi(0, 0, takardKi(1, 0, takardKi(1, 1, takardBeMind(b))))
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
      c :: oldMegLépésenként(cVégjáték)
    )
    println

    view.AknakeresőKonzolon.írdKiEgymásMellé(
      a :: oldMegLépésenként(jólElkezdettA)
    )
    println

    view.AknakeresőKonzolon.írdKiEgymásMellé(
      b :: oldMegLépésenként(jólElkezdettB)
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
    val lépés = jelöldAzAknákat andThen takardKiANemAknákat
    implicit val újTábla :: régiTáblák = lépés(régi)
    if (cellák exists takart)
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

  def keresdAzAknákat(implicit tábla: Tábla): KoordinátaLista =
    (cellák flatMap { implicit koordináták =>

      if ((nemNullaSzám getOrElse -1) == (szomszédok count takart) + (szomszédok count látszódóAkna))
        szomszédok filter takart
      else
        List()
    }).distinct

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
    val szk: Set[(Int, Int)] = koordináták match {
      case (x, y) => for (q <- ListSet(
        (x - 1, y - 1), (x, y - 1), (x + 1, y - 1),
        (x - 1, y), /*           */ (x + 1, y),
        (x - 1, y + 1), (x, y + 1), (x + 1, y + 1))) yield q
    }

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

    def loop(tábla: Tábla, c: (Int, Int)): Tábla = {
      val (x, y) = c
      tábla(y)(x) match {
        case TakartSzám(n) =>
          újTábla(tábla, x, y, Szám(n))
        case TakartAkna =>
          újTábla(tábla, x, y, Akna)
        case _ =>
          tábla
      }
    }

    tábla(kiY)(kiX) match {
      case TakartSzám(0) =>
        nullaSzomszédokSzomszédjai((kiX, kiY), tábla).foldLeft(tábla)(loop)
      case _ =>
        loop(tábla, (kiX, kiY))
    }
  }

  def nullaSzomszédokSzomszédjai(nulla: (Int, Int), tábla: Tábla): Set[(Int, Int)] = {

    //noinspection TypeAnnotation
    def loop(szomszédokEddig: Set[(Int, Int)], c: (Int, Int)): Set[(Int, Int)] = {
      if (szomszédok(c, tábla).forall(szomszédokEddig.contains))
        szomszédokEddig
      else tábla(c._2)(c._1) match {
        case TakartSzám(0) =>
          implicit val cella = c
          implicit val t = tábla
          (szomszédok + c -- szomszédokEddig).foldLeft(szomszédok + c ++ szomszédokEddig)(loop)
        case _ =>
          szomszédokEddig + c
      }
    }

    loop(Set(), nulla)
  }

  def újTábla(tábla: Tábla, x: Int, y: Int, cella: Cella): Tábla = tábla.updated(y, tábla(y).updated(x, cella))

  def takartNullaSzám(implicit tábla: Tábla): ((Int, Int)) => Boolean = {
    case ((x, y)) => tábla(y)(x) match {
      case TakartSzám(0) => true
      case _ => false
    }
  }

  def rakd(rakdX: Int, rakdY: Int, tábla: Tábla): Tábla =
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
