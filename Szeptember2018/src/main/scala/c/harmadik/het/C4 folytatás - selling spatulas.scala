
object C4SellingSpatulas {

  def main(args: Array[String]): Unit = {
    // val tartalom = scala.io.Source.fromInputStream(System.in)
    val tartalom = scala.io.Source.fromFile("Szeptember2018/src/main/scala/c/harmadik/het/Sample Input.txt")
    val sorok: Iterator[String] = tartalom.getLines()

    val boltokSoria: List[Either[(Int, Double), Int]] =
      sorok.toList.map { sor =>
        if (sor.contains(" ")) {
          val Array(perc, bevétel) = sor.split(" ")
          Left((perc.toInt, bevétel.toDouble))
        } else {
          Right(sor.toInt)
        }
      }

    következőBolt(boltokSoria)
  }

  def következőBolt(boltokSoria: List[Either[(Int, Double), Int]]): Unit = {
    boltokSoria match {

      case Right(n) :: többiSor =>
        val boltSorai: List[(Int, Double)] =
          többiSor.take(n).map(sor => sor.left.get)
        if (n > 0) {
          printMaxProfit(boltSorai)
          következőBolt(többiSor.drop(n))
        }

    }
  }

  def printMaxProfit(list: List[(Int, Double)]): Unit = {
    var maxP = Double.MinValue
    var maxEp = 0
    var maxUp = 0

    for (első <- list.indices; utolsó <- első to list.indices.last) {
      val részLista = list.slice(from = első, until = utolsó + 1)

      val bevételek = részLista.map(sor => sor._2)
      val öszBevétel = bevételek.sum

      val elsőPerc = részLista.head._1
      val utolsóPerc = részLista.last._1
      val öszKiadás = (utolsóPerc - elsőPerc + 1) * 0.08

      if (maxP < öszBevétel - öszKiadás) {
        maxP = öszBevétel - öszKiadás
        maxEp = elsőPerc
        maxUp = utolsóPerc
      }
    }

    if (maxP > 0)
      println(f"$maxP%1.2f $maxEp $maxUp")
    else
      println("no profit")
  }
}