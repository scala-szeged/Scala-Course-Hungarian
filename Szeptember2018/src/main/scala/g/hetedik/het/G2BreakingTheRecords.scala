package g.hetedik.het

// https://www.hackerrank.com/challenges/breaking-best-and-worst-records

object Solution_breakingRecords {

  // Complete the breakingRecords function below.
  def breakingRecords(scores: Array[Int]): Array[Int] = {

    if (scores.isEmpty) {
      Array(0, 0)

    } else {

      var min = scores.head
      var max = scores.head
      var minSzáma = 0
      var maxSzáma = 0

      for (s <- scores.tail) {

        if (s < min) {
          min = s
          minSzáma += 1
        }
        if (s > max) {
          max = s
          maxSzáma += 1
        }
      }

      Array(maxSzáma, minSzáma)
    }
  }

  def main(args: Array[String]) {
    println(
      breakingRecords(Array(10, 5, 20, 20, 4, 5, 2, 25, 1)).toList
    )
    /*
        val stdin = scala.Console

        val printWriter = new PrintWriter(sys.env("OUTPUT_PATH"))

        val n = stdin.readLine.trim.toInt

        val scores = stdin.readLine.split(" ").map(_.trim.toInt)
        val result = breakingRecords(scores)

        printWriter.println(result.mkString(" "))

        printWriter.close()
    */
  }
}

