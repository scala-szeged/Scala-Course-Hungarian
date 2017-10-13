package f.hatodik.het

import scala.io.Source

object F_3_3_SzövegrészletKeresése {

  def main(args: Array[String]): Unit = {
    val fájl = Source.fromFile("Szeptember2017/src/main/scala/c/harmadik/het/csomagolópapír.txt")
    val szöveg = fájl.toList

    println("részListaE(szöveg, \"10x22\".toList): " + részListaE(szöveg, "10x22".toList))
    println("részListaE(szöveg, \"kémia\".toList): " + részListaE(szöveg, "kémia".toList))
    println("részListaE(szöveg, \"251x173\".toList): " + részListaE(szöveg, "251x173".toList))
  }

  @annotation.tailrec
  def részListaE[A](aLista: List[A], bLista: List[A]): Boolean = aLista match {
    case Nil => bLista == Nil
    case _ if evvelKezdődikE(aLista, bLista) => true
    case _ :: aListaTovább => részListaE(aListaTovább, bLista)
  }

  @annotation.tailrec
  def evvelKezdődikE[A](lista: List[A], prefix: List[A]): Boolean = (lista, prefix) match {

    case (_, Nil) =>
      true

    case (l :: listaTovább, p :: prefixTovább) if l == p =>
      evvelKezdődikE(listaTovább, prefixTovább)

    case _ =>
      false
  }
}
