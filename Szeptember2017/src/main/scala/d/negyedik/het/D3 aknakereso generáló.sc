trait Cella

case object Akna extends Cella

case class Szám(n: Int) extends Cella

def írdki(tábla: List[List[Cella]]): Unit = {
  for (sor <- tábla) {
    println
    for (cella <- sor) cella match {
      case Akna => print("*")
      case Szám(n) => print(n)
    }
  }
  println
}

val üres =   List(
  List(Szám(0), Szám(0), Szám(0)),
  List(Szám(0), Szám(0), Szám(0)),
  List(Szám(0), Szám(0), Szám(0))
)

írdki(üres)