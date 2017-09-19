
// --- HTTP(S) input kezelése

val symbol = "AAPL"
val url =
  s"https://www.alphavantage.co/query?function=TIME_SERIES_WEEKLY&symbol=$symbol&apikey=A586MDCHZRS4GXCD"

val charIterator = scala.io.Source.fromURL(url)
val json = charIterator.mkString
val lineIterator = json.lines

//val date = lineIterator.toList(8)
val closing = lineIterator.toList(12)
val closingPrice = closing.split("\"")(3).toDouble
