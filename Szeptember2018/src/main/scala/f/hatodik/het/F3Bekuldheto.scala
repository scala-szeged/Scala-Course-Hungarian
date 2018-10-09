package f.hatodik.het

object F3Bekuldheto {

  def main(args: Array[String]): Unit = {
    val scan = scala.Console
    val s = scan.readLine

    val result = megszüntet(s)

    println(result)
  }

  def megszüntet(szó: String): String = {

    def loop(maradékSzó: List[Char], gyűjtött: List[Char]): List[Char] = (maradékSzó, gyűjtött) match {

      case (ez :: többiBetű, List()) =>
        loop(többiBetű, ez :: gyűjtött)

      case (ez :: többiBetű, előző :: mégEzelőttiek) if ez == előző =>
        loop(többiBetű, gyűjtött)

      case (ez :: többiBetű, előző :: mégEzelőttiek) =>
        loop(többiBetű, ez :: gyűjtött)

      case (List(), _) =>
        gyűjtött
    }

    loop(szó.toList, List()).mkString.reverse
  }
}
