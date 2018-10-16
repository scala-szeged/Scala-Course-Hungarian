package g.hetedik.het

import java.io._
import java.math._
import java.security._
import java.text._
import java.util._
import java.util.concurrent._
import java.util.function._
import java.util.regex._
import java.util.stream._

object Solution {

  // Complete the breakingRecords function below.
  def breakingRecords(scores: Array[Int]): Array[Int] = {

    var min = Int.MaxValue
    var max = Int.MinValue
    var minSzáma = 0
    var maxSzáma = 0

    for (s <- scores) {

      if (s < min) {
        min = s
        minSzáma += 1
      }
      if (s > max) {
        max = s
        maxSzáma += 1
      }
    }

    Array(maxSzáma - 1, minSzáma - 1)
  }

  def main(args: Array[String]) {
    val stdin = scala.Console

    val printWriter = new PrintWriter(sys.env("OUTPUT_PATH"))

    val n = stdin.readLine.trim.toInt

    val scores = stdin.readLine.split(" ").map(_.trim.toInt)
    val result = breakingRecords(scores)

    printWriter.println(result.mkString(" "))

    printWriter.close()
  }
}

