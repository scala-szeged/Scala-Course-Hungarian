import scala.collection.GenSeq
// --- Input


def quandlGetClosingPrice(symbol : String) = {
  val url =
    s"https://www.alphavantage.co/query?function=TIME_SERIES_WEEKLY&symbol=$symbol&apikey=A586MDCHZRS4GXCD"

  val data = scala.io.Source.fromURL(url).mkString.lines.toList
  val price = data(12).split("\"")(3).toDouble
  price
}

def findMax(symbols: GenSeq[String]): Unit = {
  val startTime = System.currentTimeMillis()
  val (topStock, topPrice) =
    symbols
      .map { symbol => (symbol, quandlGetClosingPrice(symbol)) }
      .maxBy { symbolAndPrice => symbolAndPrice._2 }
  println(s"Top stock is $topStock closing at price $$$topPrice")
  val endTime = System.currentTimeMillis()
  println(s"${endTime - startTime} milliseconds")
}
val symbols = List("GOOG","INTC","AMD","AAPL","AMZN","IBM","ORCL","MSFT")
findMax(symbols.par)
