package d.negyedik.het


object AknakeresőGeneráló {

  trait Cella

  case object Akna extends Cella

  case class Szám(n: Int) extends Cella

  type Tábla = List[List[Cella]]


  def main(args: Array[String]): Unit = {

    val üres = List(
      List(Szám(0), Szám(0), Szám(0)),
      List(Szám(0), Szám(0), Szám(0)),
      List(Szám(0), Szám(0), Szám(0))
    )

    írdKi(rakd(0,1,rakd(0, 0, üres)))
    írdKi(rakd(0, 1, üres))
    írdKi(rakd(1, 1, üres))


    def írdKi(tábla: Tábla): Unit = {
      for (sor <- tábla) {
        println
        for (cella <- sor) cella match {
          case Akna => print("*")
          case Szám(n) => print(n)
        }
      }
      println
    }

    def rakd(x: Int, y: Int, tábla: Tábla): Tábla =
      for (sorIndex <- tábla.zipWithIndex) yield
        for (cellaIndex <- sorIndex._1.zipWithIndex) yield
          (cellaIndex._1, sorIndex._2, cellaIndex._2) match {

            case (_, cx, cy) if cx == x && cy == y =>
              Akna

            case (Szám(n), cx, cy) =>
              if (Math.abs(cx - x) <= 1 && Math.abs(cy - y) <= 1) Szám(n + 1)
              else Szám(n)

            case (c, _, _) =>
              c
          }


  }
}
