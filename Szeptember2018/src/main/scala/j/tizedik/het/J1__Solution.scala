package j.tizedik.het

// Itt található a feladat: https://www.hackerrank.com/challenges/string-mingling/problem
// A regisztrált felhasználók beküldhetik a megoldást, a B1__ prefix ilyenkor törlendő.

object J1__Solution {

  def main(args: Array[String]): Unit = {
    /*
        val scan = scala.Console
        val p = scan.readLine
        val q = scan.readLine
    */
    val p = "abcde"
    val q = "pqrst"
    println(mix(p, q))
  }

  def mix(p: String, q: String): String = {
    val eredmeny =
      for (i <- 0 until p.length)
        yield "" + p(i) + q(i)

    eredmeny.mkString
  }
}
