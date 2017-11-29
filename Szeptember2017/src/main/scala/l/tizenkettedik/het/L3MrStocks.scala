package l.tizenkettedik.het

import scala.collection.parallel.ForkJoinTaskSupport
import scala.collection.parallel.immutable.ParSeq
import scala.concurrent.forkjoin.ForkJoinPool


//noinspection TypeAnnotation
object L3MrStocks extends App {

  import scala.collection.GenSeq


  def closingPrice(symbol: String): Double = {
    try {
      val url = s"http://www.google.com/finance/historical?output=csv&q=${symbol.toLowerCase}"
      val data = scala.io.Source.fromURL(url).mkString.lines.toList
      println(s"$symbol -  ${data.take(2)}")
      val price = data(1).split(",")(4).toDouble

      price

    } catch {

      case _: Throwable =>
        0
    }
  }

  def findMax(symbols: GenSeq[String], msg: String = "") = {
    val startTime = System.currentTimeMillis()

    val (topStock, topPrice) =
      symbols
        .map { symbol => (symbol, closingPrice(symbol)) }
        .maxBy { case (symbol, price) => price }

    println(s"Top stock is $topStock closing at price $$$topPrice")
    val endTime = System.currentTimeMillis()
    println(s"$msg ${endTime - startTime} milliseconds")
  }

  val symbols = List("GOOG", "INTC", "AMD", "AAPL", "ROKU",
    "GE", "BAC", "SQ", "VALE", "MOMO", "BSX", "MU",
    "AMZN", "IBM", "ORCL", "MSFT", "TSLA", "NDAQ")


  findMax(symbols, "findMax(symbols)")
  println
  findMax(symbols.par, "findMax(symbols.par)")
  println

  val parSymbols: ParSeq[String] = symbols.par
  parSymbols.tasksupport = new ForkJoinTaskSupport(new ForkJoinPool(20))
  findMax(parSymbols, "20 threads, findMax(parSymbols)")
}