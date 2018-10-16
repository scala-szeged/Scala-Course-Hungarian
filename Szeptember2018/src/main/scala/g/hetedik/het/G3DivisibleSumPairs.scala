import java.io._
import java.math._
import java.security._
import java.text._
import java.util._
import java.util.concurrent._
import java.util.function._
import java.util.regex._
import java.util.stream._

// https://www.hackerrank.com/challenges/divisible-sum-pairs/problem

object Solution_divisibleSumPairs {

  // Complete the divisibleSumPairs function below.
  def divisibleSumPairs(n: Int, k: Int, ar: Array[Int]): Int = {

    val összegek = for {
      i <- ar.indices
      j <- i + 1 until ar.size
    } yield ar(i) + ar(j)

    val jók = összegek.filter(ö => ö % k == 0)
    println(jók.toList)

    jók.size
  }

  def main(args: Array[String]) {
    println(
      divisibleSumPairs(6, 3, Array(1, 3, 2, 6, 1, 2))
    )
    /*
        val stdin = scala.Console

        val printWriter = new PrintWriter(sys.env("OUTPUT_PATH"))

        val nk = stdin.readLine.split(" ")

        val n = nk(0).trim.toInt

        val k = nk(1).trim.toInt

        val ar = stdin.readLine.split(" ").map(_.trim.toInt)
        val result = divisibleSumPairs(n, k, ar)

        printWriter.println(result)

        printWriter.close()
    */
  }
}
