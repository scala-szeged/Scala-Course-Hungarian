import scala.collection.immutable

val elvártEredmény =
  """|
     |00:00:00
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
  //.slice(0, 1)
  .map(timeConversion).foreach(println)

/**
  * Fajth Imre megoldása a legjobb elkészült megoldás. Bemásoltam ide
  * innen: https://github.com/fajthimre/Tigra-Scala-kurzus/blob/master/Szeptember2018/src/main/scala/e/otodik/het/E1.sc
  */
def timeConversion(s: String): String = {
  val time: immutable.List[String] = s.replace(":", "").grouped(2).toList
  val hour = time(0).toInt
  val minute = time(1).toInt
  val sec = time(2).toInt
  val ampm = time(3)

  if (ampm == "AM") {

    if (hour == 12) {
      hour - 12 + ":" + minute + ":" + sec
    } else {
      hour + ":" + minute + ":" + sec
    }

  } else {

    if (hour == 12) {
      hour + ":" + minute + ":" + sec
    } else {
      hour + 12 + ":" + minute + ":" + sec
    }
  }
}