package f.hatodik.het


// Itt található a feladat: https://open.kattis.com/problems/apaxiaaans
// A regisztrált felhasználók beküldhetik a megoldást.
object F3Bekuldheto {

  def main(args: Array[String]): Unit = {
    val scan = scala.Console
    val s = scan.readLine

    val result = megszüntet(s)

    println(result)
  }

  // A feladat
  def megszüntet(szó: String): String = {

    def loop(maradékSzó: List[Char], gyűjtött: List[Char]): List[Char] = (maradékSzó, gyűjtött) match {

      case (ez :: többiBetű, List()) =>
        loop(többiBetű, List(ez))

      case (List(), mindenAmitGyűjtöttünk) =>
        mindenAmitGyűjtöttünk

      case (ez :: többiBetű, előző :: előzőElőttiBetűk) if ez == előző =>
        loop(többiBetű, előző :: előzőElőttiBetűk)

      case (ez :: többiBetű, előző :: előzőElőttiBetűk) if ez != előző =>
        loop(többiBetű, ez :: előző :: előzőElőttiBetűk)
    }

    val eredmény = loop(szó.toList, List())
    eredmény.reverse.mkString
  }
}
