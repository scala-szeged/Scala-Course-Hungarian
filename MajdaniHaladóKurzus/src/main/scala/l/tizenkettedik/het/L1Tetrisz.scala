package l.tizenkettedik.het

import java.lang.Math.{max, min}

object L1Tetrisz {

  type Pálya = List[List[String]]
  type Lépés = (List[Pálya], (Int, Int), Pálya, List[Pálya]) => (List[Pálya], (Int, Int), Pálya, List[Pálya])

  val pályaMagasság = 6
  val kezdőPont: (Int, Int) = (2, 0)

  val felsőSor = List("X", " ", " ", " ", " ", "X")
  val alsóSor = List("X", "X", "X", "X", "X", "X")

  private val üresPálya = List.fill(pályaMagasság)(felsőSor) ::: List(alsóSor)


  def main(args: Array[String]): Unit = {
    val elemek = List(
      List(List("a", "b"), List(null, "c")),
      List(List("A", "B"), List("C", "D")),
      List(List(null, "E"), List(null, "F"))
    )

    írdKiEgymásMellé(
      lépj(elemek, üresPálya,

        semmi, forduljBalra, forduljBalra, jobbra,
        //ejtsd,
        balra, semmi, semmi, semmi, semmi, semmi,


        jobbra, semmi, semmi, semmi, semmi, semmi
      )
    )
  }


  def lépj(elemek: List[Pálya], pálya: Pálya, lépések: Lépés*): List[Pálya] = {
    val elsőPálya = rakdRá(elemek.head, kezdőPont, pálya)
    val (_, _, _, pályaLista) = lépések.foldLeft((elemek, kezdőPont, pálya, List(elsőPálya))) {

      case ((Nil, (x, y), háttér, pályák), _) =>
        (Nil, (x, y), háttér, pályák)

      case ((ek, (x, y), háttér, pályák), lépés) =>
        lépés(ek, (x, y), háttér, pályák)
    }

    pályaLista.reverse.zip(semmi :: lépések.toList).map {
      case (p, `forduljBalra`) => p ::: List(List("  ↺   "))
      case (p, `forduljJobbra`) => p ::: List(List("  ↻  "))
      case (p, `balra`) => p ::: List(List("  ←   "))
      case (p, `jobbra`) => p ::: List(List("  →   "))
      case (p, `semmi`) => p ::: List(List("      "))
      case (p, `ejtsd`) => p ::: List(List(" ↓  ↓ "))
    }
  }

  //noinspection ScalaUnusedSymbol
  val földetért: (List[Pálya], (Int, Int), Pálya) => Boolean = {
    case (Nil, hol: (Int, Int), háttér: Pálya) =>
      false

    case (elem :: _, (x, y), háttér: Pálya) =>
      val alsóSor = elem.last
      alsóSor.zipWithIndex.exists {
        case (null, _) =>
          false

        case (_, index) =>
          val strings = háttér(y + elem.size)
          strings(x + index) != " "
      }
  }

  val forduljBalra: Lépés = {
    case (e :: többielem, (x, y), háttér: Pálya, pályák: List[Pálya]) =>
      tovább(e.map(_.reverse).transpose :: többielem, (x, y + 1), háttér, pályák)
  }

  val forduljJobbra: Lépés = {
    case (e :: többielem, (x, y), háttér: Pálya, pályák: List[Pálya]) =>
      tovább(e.transpose.map(_.reverse) :: többielem, (x, y + 1), háttér, pályák)
  }

  val jobbra: Lépés = {
    case (e :: többielem, (x, y), háttér: Pálya, pályák: List[Pálya]) =>
      tovább(e :: többielem, (x + 1, y + 1), háttér, pályák)
  }

  val balra: Lépés = {
    case (e :: többielem, (x, y), háttér: Pálya, pályák: List[Pálya]) =>
      tovább(e :: többielem, (x - 1, y + 1), háttér, pályák)
  }

  val semmi: Lépés = {
    case (e :: többielem, (x, y), háttér: Pálya, pályák: List[Pálya]) =>
      tovább(e :: többielem, (x, y + 1), háttér, pályák)
  }

  val ejtsd: Lépés = {
    case (elemek: List[Pálya], (x, y), háttér: Pálya, pályák: List[Pálya]) =>

      if (földetért(elemek, (x, y + 1), háttér)) {
        újElemFentről(elemek, (x, y + 1), háttér, pályák)

      } else {

        val _tovább = tovább(elemek, (x, y + 1), háttér, pályák)
        ejtsd.tupled(_tovább)
      }
  }

  lazy val tovább: Lépés = {
    case (Nil, hova, háttér: Pálya, pályák: List[Pálya]) =>
      (Nil, hova, háttér, pályák)

    case (elemek, hova, háttér: Pálya, pályák: List[Pálya]) if földetért(elemek, hova, háttér) =>
      újElemFentről(elemek, hova, háttér, pályák)

    case (e :: többiElem, (újX, újY), háttér: Pálya, pályák: List[Pálya]) =>
      val hova = (
        min(max(újX, 1), üresPálya.head.size - 3),
        min(max(újY, 0), üresPálya.size - 2)
      )
      val újPálya = rakdRá(e, hova, háttér)
      (e :: többiElem, hova, háttér, újPálya :: pályák)
  }

  lazy val újElemFentről: Lépés = {
    case (e1 :: e2 :: többiElem, hova: (Int, Int), háttér: Pálya, pályák: List[Pálya]) =>
      val h = rakdRá(e1, hova, háttér)
      val újHáttér = if (h.dropRight(1).last.contains(" ")) h else felsőSor :: h.dropRight(2) ::: List(alsóSor)
      val újPálya = rakdRá(e2, kezdőPont, újHáttér)
      (többiElem, kezdőPont, újHáttér, újPálya :: pályák)

    case (_, hova, háttér, pályák) =>
      (Nil, hova, háttér, pályák)
  }

  def rakdRá(elem: Pálya, hova: (Int, Int), pálya: Pálya): Pálya = {
    val (x, y) = hova
    elem.zipWithIndex.foldLeft(pálya) {
      case (p, (eSor, sor)) =>
        p.updated(sor + y, eSor.zipWithIndex.foldLeft(p(sor + y)) {
          case (pSor, (null, _)) => pSor
          case (pSor, (e, oszlop)) => pSor.updated(oszlop + x, e)
        })
    }
  }

  def írdKiEgymásMellé(pályák: Pálya*): Unit = írdKiEgymásMellé(pályák.toList)

  def írdKiEgymásMellé(pályák: List[Pálya]): Unit = {
    val elválasztó = "   "

    print(
      pályák.transpose.map(_.map(_.mkString).mkString(elválasztó)).mkString("\n")
    )
  }

  def írdKi(pálya: Pálya): Unit = println(pálya.map(_.mkString).mkString("\n"))
}
