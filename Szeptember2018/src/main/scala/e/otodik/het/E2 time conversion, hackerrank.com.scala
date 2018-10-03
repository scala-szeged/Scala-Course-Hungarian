// https://www.hackerrank.com/challenges/time-conversion/problem

object Solution {

  /*
   * Complete the timeConversion function below.
   */
  def timeConversion(s: String): String = {
    s.replace(":", "").grouped(2).toList match {

      case "12" :: min :: sec :: "AM" :: Nil =>
        s"00:$min:$sec"

      case hour :: min :: sec :: "AM" :: Nil =>
        s"$hour:$min:$sec"

      case "12" :: min :: sec :: "PM" :: Nil =>
        s"12:$min:$sec"

      case hour :: min :: sec :: "PM" :: Nil =>
        s"${hour.toInt + 12}:$min:$sec"
    }

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
