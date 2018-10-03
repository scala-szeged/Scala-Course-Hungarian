// https://www.hackerrank.com/challenges/time-conversion/problem

object Solution {

  /*
   * Complete the timeConversion function below.
   */
  def timeConversion(s: String): String = {
    s
  }

  def main(args: Array[String]) {

    // https://en.wikipedia.org/wiki/12-hour_clock
    List(
      "12:00:00AM"
      , "12:01:00AM"
      , "01:00:00AM"
      , "11:00:00AM"
      , "11:59:00AM"
      , "12:00:00PM"
      , "12:01:00PM"
      , "01:00:00PM"
      , "11:00:00PM"
      , "11:59:00PM"
    ).map(timeConversion).foreach(println)
    /*
        val scan = scala.io.StdIn
        val fw = new PrintWriter(sys.env("OUTPUT_PATH"))
        val s = scan.readLine

        val result = timeConversion(s)

        fw.println(result)
        fw.close()
    */
  }
}
