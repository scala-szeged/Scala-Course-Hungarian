
// --- Case class, 3. hét


case class Sale(minutes: Int, profit: Double)

case class Store(sales: List[Sale])


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

val SaleLine ="""(\d+) (\d+\.\d\d)""".r

val lines = input.lines.toList.tail

def collect(lines: List[String], stores: List[Store]): List[Store] = {
  val n = lines.head.toInt
  if (n > 0) {
    val sales = lines.tail.take(n).map {
      case SaleLine(m, p) => Sale(m.toInt, p.toDouble)
    }
    collect(lines.tail.drop(n), Store(sales) :: stores)

  } else {

    stores
  }
}

val list = collect(lines, Nil)
