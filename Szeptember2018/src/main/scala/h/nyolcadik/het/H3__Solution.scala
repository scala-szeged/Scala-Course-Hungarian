package h.nyolcadik.het

import java.io.PrintWriter

// Itt található a feladat: https://www.hackerrank.com/challenges/repeated-string/problem?h_l=interview&playlist_slugs%5B%5D=interview-preparation-kit&playlist_slugs%5B%5D=warmup
// A regisztrált felhasználók beküldhetik a megoldást, a B1__ prefix ilyenkor törlendő.

object H3__Solution {

  def main(args: Array[String]): Unit = {
    /*
            val stdin = scala.Console
            val printWriter = new PrintWriter(sys.env("OUTPUT_PATH"))
            val s = stdin.readLine
            val n = stdin.readLine.trim.toLong
            val result = számoldAzAkat(s, n)
            printWriter.println(result)
            printWriter.close()
    */

    teszt1()
    teszt2()
  }

  def számoldAzAkat(str: String, n: Int): Int = {
    0
  }

  def teszt1(): Unit = {
    val str = "aba"
    val n = 10
    println("Elvárt eredmény: 7")
    println(számoldAzAkat(str, n))
  }

  def teszt2(): Unit = {
    val str = "abcac"
    val n = 10
    println("Elvárt eredmény: 4")
    println(számoldAzAkat(str, n))
  }
}
