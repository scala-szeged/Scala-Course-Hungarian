package l.tizenkettedik.het

import java.lang.Math.{max, min}

object L1Tetrisz {

  type Pálya = List[List[String]]
  type Lépés = (Pálya, (Int, Int)) => (Pálya, (Int, Int))

  val pályaMagasság = 6
  val kezdőPont: (Int, Int) = (2, 0)

  val felsőSor = List("X", " ", " ", " ", " ", "X")
  val alsóSor = List("X", "X", "X", "X", "X", "X")


  def main(args: Array[String]): Unit = {
    val elemek = List(
      List(List("a", "b"), List(null, "c")),
      List(List("A", "B"), List("C", "D")),
      List(List(null, "E"), List(null, "F"))
    )
    val pálya = List.fill(pályaMagasság)(felsőSor) ::: List(alsóSor)

    írdKiEgymásMellé(
      lépj(elemek, pálya,
        ejtsd,
        // HIBÁT IDÉZ ELŐ: ejtsd
        semmi, forduljBalra, forduljBalra, jobbra,
        semmi, semmi, balra, semmi, semmi, semmi,
        // HIBÁT IDÉZ ELŐ: ejtsd
        jobbra, semmi, semmi, semmi, semmi, semmi
      )
    )
  }


  def lépj(elemek: List[Pálya], pálya: Pálya, lépések: Lépés*): List[Pálya] = {
    val elsőPálya = rakdRá(elemek.head, kezdőPont, pálya)

    val (_, _, _, pályaLista) = lépések.foldLeft((elemek, kezdőPont, pálya, List(elsőPálya))) {

      case ((e :: többiElem, (x, y), háttér, pályák), `ejtsd`) =>
        ejtsdLe(e, többiElem, kezdőPont, pálya, List(elsőPálya))

      case ((e1 :: többiElem, hova, háttér, pályák), lépés) if földetért(e1, hova, háttér) =>
        következő(e1, többiElem, hova, háttér, pályák)

      case ((e :: et, (x, y), háttér, pályák), lépés) =>
        val (újElem, (újX, újY)) = lépés(e, (x, y))
        val hova = (
          min(max(újX, 1), pálya.head.size - 3),
          min(max(újY + 1, 0), pálya.size - 2)
        )
        val újPálya = rakdRá(újElem, hova, háttér)
        (újElem :: et, hova, háttér, újPálya :: pályák)

      case ((Nil, (x, y), háttér, pályák), _) =>
        (Nil, (x, y), háttér, pályák)
    }
    pályaLista.reverse.zip(lépések).map {
      case (p, `forduljBalra`) => p ::: List(List("  ↺   "))
      case (p, `forduljJobbra`) => p ::: List(List("  ↻  "))
      case (p, `balra`) => p ::: List(List("  ←   "))
      case (p, `jobbra`) => p ::: List(List("  →   "))
      case (p, `semmi`) => p ::: List(List("      "))
      case (p, `ejtsd`) => p ::: List(List(" ↓  ↓ "))
    }
  }

  def földetért(elem: Pálya, hol: (Int, Int), háttér: Pálya): Boolean = {
    val alsóSor = elem.last
    alsóSor.zipWithIndex.exists {
      case (null, _) =>
        false

      case (_, index) =>
        val (x, y) = hol
        háttér(y + elem.size)(x + index) != " "
    }
  }

  val forduljBalra: Lépés = (elem, hova) => (elem.map(_.reverse).transpose, hova)

  val forduljJobbra: Lépés = (elem, hova) => (elem.transpose.map(_.reverse), hova)

  val jobbra: Lépés = {
    case (elem, (x, y)) => (elem, (x + 1, y))
  }

  val balra: Lépés = {
    case (elem, (x, y)) => (elem, (x - 1, y))
  }

  val semmi: Lépés = {
    case (elem, (x, y)) => (elem, (x, y))
  }

  val ejtsd: Lépés = {
    case (elem, (x, y)) => (elem, (x, y))
  }

  def ejtsdLe(elem: Pálya, többiElem: List[Pálya], honnan: (Int, Int), háttér: Pálya, pályák: List[Pálya])
  : (List[Pálya], (Int, Int), Pálya, List[Pálya])
  = {
    if (földetért(elem, honnan, háttér)) {
      következő(elem, többiElem, honnan, háttér, pályák)

    } else {

      val (x, y) = honnan
      ejtsdLe(elem, többiElem, (x, y + 1), háttér, pályák)
    }
  }

  def következő(e1: Pálya, többiElem: List[Pálya], hova: (Int, Int), háttér: Pálya, pályák: List[Pálya])
  : (List[Pálya], (Int, Int), List[List[String]], List[Pálya])
  = {
    if (többiElem.nonEmpty) {
      val e2 = többiElem.head
      val h = rakdRá(e1, hova, háttér)
      val újHáttér = if (h.dropRight(1).last.contains(" ")) h else felsőSor :: h.dropRight(2) ::: List(alsóSor)
      val újPálya = rakdRá(e2, kezdőPont, újHáttér)
      (többiElem, kezdőPont, újHáttér, újPálya :: pályák)
    } else {
      (Nil, hova, háttér, pályák)
    }
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

    println(
      pályák.transpose.map(_.map(_.mkString).mkString(elválasztó)).mkString("\n")
    )
  }

  def írdKi(pálya: Pálya): Unit = println(pálya.map(_.mkString).mkString("\n"))
}
