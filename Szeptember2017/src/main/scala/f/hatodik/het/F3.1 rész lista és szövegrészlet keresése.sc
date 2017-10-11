
def részListaE[A](aElőlről: List[A], bElőlről: List[A]): Boolean = {

  def loop(aLista: List[A], bLista: List[A]): Boolean =
    (aLista, bLista) match {

      case (a :: _, b :: Nil) if a == b =>
        true

      case (Nil, _) =>
        false

      case (_, Nil) =>
        true

      case (a :: aTovább, b :: bTovább) =>
        if (a == b) {
          loop(aTovább, bTovább) || loop(aTovább, bElőlről)
        } else {
          loop(aTovább, bElőlről)
        }
    }

  loop(aElőlről, bElőlről)
}

részListaE(Nil, Nil)
// pontosan ezt jelenti: részListaE(List(), List())

részListaE("minden nap".toList, "naknap".toList)
részListaE("minden nap".toList, "na".toList)
részListaE("minden nap".toList, "nak".toList)
részListaE("minden nap".toList, "nap".toList)
részListaE("minden nap".toList, "kin".toList)
részListaE("minden nap".toList, "en na".toList)
