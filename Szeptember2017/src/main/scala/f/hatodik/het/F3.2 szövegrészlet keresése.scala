package f.hatodik.het

import scala.io.Source

object F_3_2_SzövegrészletKeresése {

  def main(args: Array[String]): Unit = {
    val fájl = Source.fromFile("Szeptember2017/src/main/scala/c/harmadik/het/csomagolópapír.txt")
    val szöveg = fájl.toList

    println("részListaE(szöveg, \"10x22\".toList): " + részListaE(szöveg, "10x22".toList))
    println("részListaE(szöveg, \"kémia\".toList): " + részListaE(szöveg, "kémia".toList))
    println("részListaE(szöveg, \"251x173\".toList): " + részListaE(szöveg, "251x173".toList))
  }

  def részListaE[A](aElőlről: List[A], bElőlről: List[A]): Boolean = {

    def loop(aLista: List[A], bLista: List[A]): Boolean =
      (aLista, bLista) match {

        case (a :: _, b :: Nil) if a == b =>
          true

        case (_, Nil) =>
          true

        case (Nil, _) =>
          false

        case (a :: aTovább, b :: bTovább) =>
          if (a == b) {
            loop(aTovább, bTovább)
          } else if (a == bElőlről.head) {
            loop(a :: aTovább, bElőlről)
          } else {
            loop(aTovább, bElőlről)
          }
      }

    loop(aElőlről, bElőlről)
  }
}

/*
Alább található az a változat, amit a https://github.com/kenbot/free projektbe belehelyezve lehet kipróbálni
és stack helyett heap -et használ:

  def részListaE[A](aElőlről: List[A], bElőlről: List[A]): Boolean = {

    def loop(aLista: List[A], bLista: List[A]): Trampoline[Boolean] =
      (aLista, bLista) match {

        case (a :: _, b :: Nil) if a == b =>
          Return(true)

        case (Nil, _) =>
          Return(false)

        case (_, Nil) =>
          Return(true)

        case (a :: aTovább, b :: bTovább) =>
          if (a == b) {
            Suspend(() => loop(aTovább, bTovább))
          } else if (a == bElőlről.head) {
            Suspend(() => loop(a :: aTovább, bElőlről))
          } else {
            Suspend(() => loop(aTovább, bElőlről))
          }
      }

    loop(aElőlről, bElőlről).run
  }
*/