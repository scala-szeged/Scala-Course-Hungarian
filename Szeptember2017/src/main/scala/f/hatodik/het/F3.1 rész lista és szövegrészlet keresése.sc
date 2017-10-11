
def részListaE[A](aElőlről: List[A], bElőlről: List[A]): Boolean = {

  @annotation.tailrec
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

részListaE(Nil, Nil)
// pontosan ezt jelenti: részListaE(List(), List())

részListaE("miminden volt ma?".toList, "minden".toList)

részListaE("minden nap".toList, "naknap".toList)
részListaE("minden nap".toList, "na".toList)
részListaE("minden nap".toList, "nak".toList)
részListaE("minden nap".toList, "nap".toList)
részListaE("minden nap".toList, "kin".toList)
részListaE("minden nap".toList, "en na".toList)
