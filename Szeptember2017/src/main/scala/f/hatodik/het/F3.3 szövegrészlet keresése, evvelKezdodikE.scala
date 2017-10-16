package f.hatodik.het

import scala.io.Source

object F_3_3_SzövegrészletKeresése {

  def main(args: Array[String]): Unit = {

    val lista = List(1, 2, 3, 4)

    val része0 = List()
    val része1 = List(1, 2, 3, 4)
    val része2 = List(1, 2)
    val része3 = List(2)
    val része4 = List(2, 3, 4)

    val nemRésze1 = List(1, 2, 3, 4, 9)
    val nemRésze2 = List(9, 1, 2)
    val nemRésze3 = List(9)
    val nemRésze4 = List(2, 9, 3, 4)

    println("részListaE(lista,része0)", részListaE(lista, része0))
    println("részListaE(lista,nemRésze1)", részListaE(lista, nemRésze1))
    println("részListaE(lista,része1)", részListaE(lista, része1))
    println("részListaE(lista,nemRésze2)", részListaE(lista, nemRésze2))
    println("részListaE(lista,része2)", részListaE(lista, része2))
    println("részListaE(lista,nemRésze3)", részListaE(lista, nemRésze3))
    println("részListaE(lista,része3)", részListaE(lista, része3))
    println("részListaE(lista,nemRésze4)", részListaE(lista, nemRésze4))
    println("részListaE(lista,része4)", részListaE(lista, része4))

    szövegKeresése
  }

  def szövegKeresése: Unit = {
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
