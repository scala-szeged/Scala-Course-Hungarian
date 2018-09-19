
/*
https://open.kattis.com/problems/sellingspatulas

  ... A store should be open when it is most profitable,
  so you want to write a program that determines the
  single period that is most profitable for each store
  individually. There is a constant cost of $0.08 per
  minute to keep each store running while it is open,
  and you assume there are no costs associated with
  keeping a store closed.

    ... Each sale has the number of minutes after
    opening (in the range 0 to 1439) and the profit
    (a real number in the range [−100,100] with two
    digits after the decimal).

      ... For each store, print the maximum profit
      (as a real number with two digits past the decimal
      point), followed by the beginning and ending
      minutes when the profit is realized.
 */
object C2SellingSpatulas {

  def main(args: Array[String]): Unit = {
    val tartalom = scala.io.Source.fromFile("Szeptember2018/src/main/scala/c/harmadik/het/Sample Input.txt")
    val sorok: Iterator[String] = tartalom.getLines()

    val boltokSoria = sorok.toList.map { sor =>
      if (sor.contains(" ")) {
        val Array(perc, bevétel) = sor.split(" ")
        Left((perc.toInt, bevétel.toDouble))
      } else {
        Right(sor.toInt)
      }
    }

    következőSor(boltokSoria)
  }

  def következőSor(boltokSoria: List[Either[(Int, Double), Int]]): Unit = {
    boltokSoria match {
      case Right(n) :: többiSor =>
        println(n)
        következőSor(többiSor)

      case Left((perc, bevétel)) :: többiSor =>
        println(perc, bevétel)
        következőSor(többiSor)

      case List() => // nem csinálunk semmit, vége a rekurziónak
    }
  }
}