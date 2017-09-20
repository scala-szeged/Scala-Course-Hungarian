
// --- Case class, 3. hét

// mindkettő ugyanaz
Nil
List()

// Int - egész számok
// toInt - egész számokká konvertálás

// Double - tört számok
// toDouble - tört számokká konvertálás



// Több soros String ahol stripMargin a sorok
// elejét a | jelekig magukat a | jeleket is beleértve törli

val input = """
            |2
            |15 0.37
            |36 3.51
          """.stripMargin

case class Sale(minutes: Int, profit: Double)

val SaleLine ="""(\d+) (\d+\.\d\d)""".r

val lines = input.lines.toList.tail

val n = lines.head.toInt
val sales = lines.tail.take(n).map {
  case SaleLine(m, p) => Sale(m.toInt, p.toDouble)
}
