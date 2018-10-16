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

// https://www.hackerrank.com/challenges/breaking-best-and-worst-records

object Solution_breakingRecords {

  // Complete the breakingRecords function below.
  def breakingRecords(scores: Array[Int]): Array[Int] = {

    scores
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

