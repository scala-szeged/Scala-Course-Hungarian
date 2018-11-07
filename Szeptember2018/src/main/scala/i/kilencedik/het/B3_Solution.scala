package i.kilencedik.het

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

  def prepare(str: String): Map[Int, Int] =
    str.split(" ").filter(_.nonEmpty).map(_.toInt).grouped(2).
      map(arr => arr(0) -> arr(1)).toMap

  def calc(gcd: Map[Int, Int], line: Map[Int, Int]): Map[Int, Int] =
    for ((prime, n) <- gcd if line.contains(prime)) yield prime -> Math.min(n, line(prime))
}
