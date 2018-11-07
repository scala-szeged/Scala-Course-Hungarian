package h.nyolcadik.het

// Itt található a feladat: https://www.hackerrank.com/challenges/lists-and-gcd/problem
// A regisztrált felhasználók beküldhetik a megoldást, a B4__ prefix ilyenkor törlendő.

object B3_Solution {

  def main(args: Array[String]): Unit = {
    val scan = scala.Console
    val q = scan.readLine

    val result = (1 to q.toInt).foldLeft(Map[Int, Int]()) {

      case (gcd, _) if gcd.isEmpty =>
        prepare(scan.readLine)

      case (gcd, _) =>
        val numbers = prepare(scan.readLine)
        calc(gcd, numbers)
    }
    println(result.toList.sorted.flatMap { case (prime, n) => List(prime, n) }.mkString(" "))
  }

  def prepare(str: String): Map[Int, Int] = {

  }

  def calc(gcd: Map[Int, Int], line: Map[Int, Int]): Map[Int, Int] = {

  }
}