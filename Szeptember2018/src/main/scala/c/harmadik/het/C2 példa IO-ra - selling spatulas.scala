
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

      case List() =>
    }
  }
}