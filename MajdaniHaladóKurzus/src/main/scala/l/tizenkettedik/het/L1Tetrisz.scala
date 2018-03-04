package l.tizenkettedik.het

import java.lang.Math.{max, min}

object L1Tetrisz {

  val pályaMagasság = 6
  val kezdőPont: (Int, Int) = (2, 0)

  type Pálya = List[List[String]]
  type Lépés = (Pálya, (Int, Int)) => (Pálya, (Int, Int))

  def main(args: Array[String]): Unit = {
    val elemek = List(
      List(List("a", "b"), List(null, "c")),
      List(List("A", "B"), List(null, "C"))
    )
    val pálya = List.fill(pályaMagasság)(List("X", " ", " ", " ", " ", "X")) :::
      List(List("X", "X", "X", "X", "X", "X"))

    val pályaB = List(List("1", "2", "3"), List("4", "5", "6"), List("7", "8", "9"))

    írdKiEgymásMellé(
      lépj(elemek, pálya,
        semmi, forduljBalra, forduljBalra, jobbra,
        semmi, semmi, balra, semmi, /* ↻ ↺  ← →  ↓ */ semmi, semmi)
    )
  }


  def lépj(elemek: List[Pálya], pálya: Pálya, lépések: Lépés*): List[Pálya] = {
    val elsőPálya = rakdRá(elemek.head, kezdőPont, pálya)
    val (_, _, _, pályaLista) = lépések.foldLeft((elemek, kezdőPont, pálya, List(elsőPálya))) {

      case ((e1 :: többiElem, (x, y), háttér, pályák), lépés) if földetért(e1, x, y, háttér) =>
        if (többiElem.nonEmpty) {
          val e2 = többiElem.head
          val újHáttér = rakdRá(e1, (x, y), háttér)
          val újPálya = rakdRá(e2, kezdőPont, újHáttér)
          (többiElem, kezdőPont, újHáttér, újPálya :: pályák)
        } else {
          (Nil, (x, y), háttér, pályák)
        }

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

  def földetért(elem: Pálya, x: Int, y: Int, háttér: Pálya): Boolean = {
    val alsóSor = elem.last
    alsóSor.zipWithIndex.exists {
      case (null, _) =>
        false

      case (_, index) =>
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


  //  def írdKi(pálya: List[List[Any]]): Unit = println(pálya.map(_.mkString * 2).mkString("\n"))
}
