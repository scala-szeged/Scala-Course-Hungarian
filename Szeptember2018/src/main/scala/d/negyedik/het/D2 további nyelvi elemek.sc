
// Típus paraméter a nagy A []-ek között a név után
trait ListKezelő[A] {

  def részListaE(teljesElőlről: List[A], részElőlről: List[A]): Boolean = {

    // match és case ágak. Ebből áll a pattern matching.

    def loop(aTeljes: List[A], bRész: List[A]): Boolean =
      (aTeljes, bRész) match { // 2 elemű tuple amit vizsgáltatunk a match -vel

        // case (<legalább egy elem>, <az utolsó elem, utánna üres lista>)
        // az if -nek is teljesülnie kell, ahhoz hogy ez a case hajtódjon végre
        case (a :: _, b :: List()) if a == b =>
          ???

        // case (<akárhány elem>, <üres lista>)
        case (_, List()) =>
          ???

          // csak annyit várunk el, hogy legyen legalább 1 elem
          // mindkét listában. Kettédaraboljuk őket 0. elemre és
          // a tpvábbi elemek listájára
        case (a :: aTovább, b :: bTovább) =>
          ???
      }

    loop(teljesElőlről, részElőlről)
  }
}


// Az object -ek egyetlen példány -ban használt class -ok
// object -nek úgyanúgy lehet őse mint a class -oknak.
// Több trait illetve egyetlen class lehet az őse
object IntListKezelő extends ListKezelő[Int]

class CharListKezelő extends ListKezelő[Char]


// Az object -ek modulként viselkednek és a
// bennük lévő elemek import paranccsal a jelenlegi
// namespace -be emelhetőek
import IntListKezelő._

println(
  részListaE(List(2, 3, 4, 5), List(3, 4))
)
