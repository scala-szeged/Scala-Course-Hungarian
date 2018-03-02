package l.tizenkettedik.het

import java.lang.Math.{max, min}

object L1Tetrisz {

  type Pálya = List[List[String]]
  type Lépés = (Pálya, (Int, Int)) => (Pálya, (Int, Int))

  def main(args: Array[String]): Unit = {
    val elem = List(List("a", "b"), List(null, "c"))
    val pálya = List.fill(6)(List("X", " ", " ", " ", " ", "X")) ::: List(List("X", "X", "X", "X", "X", "X"))
    val pályaB = List(List("1", "2", "3"), List("4", "5", "6"), List("7", "8", "9"))

    írdKiEgymásMellé(
      lépj(elem, pálya, forduljBalra, jobbra, jobbra)
    )
  }

  def lépj(elem: Pálya, pálya: Pálya, lépések: Lépés*): List[Pálya] = {
    val kezdőPont = (2, 0)
    val (_, _, pályaLista) = lépések.foldLeft((elem, kezdőPont, List(pálya))) {
      case ((e, (x, y), pályák), lépés) =>
        val (újElem, (újX, újY)) = lépés(e, (x, y))
        val hova = (
          min(max(újX, 1), pálya.head.size - 3),
          min(max(újY + 1, 0), pálya.size - 2)
        )
        val újPálya = rakdRá(újElem, hova, pálya)

        (újElem, hova, újPálya :: pályák)
    }
    rakdRá(elem, kezdőPont, pálya) :: pályaLista.reverse.drop(1)
  }

  def forduljBalra(elem: Pálya, hova: (Int, Int)): (Pálya, (Int, Int)) = (elem.map(_.reverse).transpose, hova)

  val forduljJobbra: Lépés = (elem, hova) => (elem.transpose.map(_.reverse), hova)

  val jobbra: Lépés = {
    case (elem, (x, y)) => (elem, (x + 1, y))
  }

  val balra: Lépés = {
    case (elem, (x, y)) => (elem, (x - 1, y))
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
