package d.negyedik.het

trait ListKezelő[A] {
  def részListaE(teljesElőlről: List[A], részElőlről: List[A]): Boolean = {

    def loop(aTeljes: List[A], bRész: List[A]): Boolean =
      (aTeljes, bRész) match {

        case (a :: _, b :: List()) if a == b =>
          true

        case (_, List()) =>
          true

        case (List(), _) =>
          false

        case (a :: aTovább, b :: bTovább) =>
          if (a == b) {
            loop(aTovább, bTovább)
          } else if (a == részElőlről.head) {
            loop(a :: aTovább, részElőlről)
          } else {
            loop(aTovább, részElőlről)
          }
      }

    loop(teljesElőlről, részElőlről)
  }
}


object IntListKezelő extends ListKezelő[Int]

class CharListKezelő extends ListKezelő[Char]


object StringKezelő {

  val charListKezelő = new CharListKezelő

  def subStringE(teljes: String, rész: String): Boolean =
    charListKezelő.részListaE(teljes.toList, rész.toList)
}


object D3RészListaE {

  import IntListKezelő._
  import StringKezelő._

  def main(args: Array[String]): Unit = {
    println(
      részListaE(List(2, 3, 4, 5), List(3, 4))
    )
    println(
      részListaE(List(4, 5), List(3, 4))
    )
    println(
      subStringE("Valamelyik ember", "ember")
    )
  }

}
