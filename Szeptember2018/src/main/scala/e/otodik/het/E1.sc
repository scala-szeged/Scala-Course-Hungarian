val elvártEredmény =
  """00:00:00
    |00:01:00
    |01:00:00
    |11:00:00
    |11:59:00
    |12:00:00
    |12:01:00
    |13:00:00
    |23:00:00
    |23:59:00""".stripMargin

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
)
  .slice(0, 1)
  .map(timeConversion).foreach(println)

def timeConversion(s: String): String = {
  val time = s.replace(":", "").grouped(2).toList
  println(time)
  s
}
