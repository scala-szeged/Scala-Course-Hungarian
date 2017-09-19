
// --- fájl input / output kezelése



case class Sale(minutes: Int, profit: Double)

case class Store(sales: List[Sale])


/*
class SellingSpatulas extends FlatSpec with Matchers {

  "it" should "print" in {
    val input =
      """
        |5
        |15 0.37
        |36 3.51
        |65 3.72
        |72 1.89
        |120 0.20
        |6
        |28 0.38
        |140 4.90
        |313 2.77
        |400 4.32
        |446 2.54
        |485 1.97
        |2
        |1 0.08
        |2 0.03
        |0
      """.stripMargin

    val expected =
      """
        |6.16 36 72
        |4.82 140 140
        |no profit
      """.stripMargin

    val SaleLine ="""(\d+) (\d+\.\d\d)""".r

    val lines = input.lines.toList.tail

    @tailrec
    def collect(lines: List[String], stores: List[Store]): List[Store] = {
      val n = lines.head.toInt
      val list = lines.tail.take(n).map {
        case SaleLine(m, p) => Sale(m.toInt, p.toDouble)
      }
      if (n > 0) collect(lines.tail.drop(n), stores ::: List(Store(list)))
      else stores
    }

    val s = collect(lines, Nil)
    val stores = Stream.continually(s).flatten.take(3)
    //    stores.foreach(println)

    def p1[X, Y](x: X, y: Y): (X, Y) = {
      //      println(s"$x, $y")
      (x, y)
    }

    for (store <- stores) {
      val subLists = for {
        i <- 1 to store.sales.size
        subList <- (List.fill(i - 1)(Sale(0, 0)) ::: store.sales).sliding(i)
      } yield p1(subList, calcProfit(subList))

      val max = subLists.maxBy(subList => 1500 * 100 * subList._2 + subList._1.head.minutes)
      //      println
      //      println
      if (max._2 > 0) println(s"${max._2} ${max._1.head.minutes} ${max._1.last.minutes}")
      else println("no profit")
      //      println
    }


  }
  val costPerMinute = 0.08

  def calcProfit(sales: List[Sale]): Double = {
    twoDigits(sales.map(_.profit).sum - costPerMinute * (sales.last.minutes - sales.head.minutes + 1))
  }

  def twoDigits(x: Double): Double = BigDecimal(x).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble
}*/
