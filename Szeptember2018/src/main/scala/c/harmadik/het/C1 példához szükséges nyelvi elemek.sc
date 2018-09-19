
// beolvasás előkészítése a standard inputról,
// ahova a felhasználó gépel
// val tartalom = scala.io.Source.fromInputStream(System.in)

// beolvasás előkészítése fájlból
// val tartalom = scala.io.Source.fromFile("Sample Input.txt")

// beolvasás soronként
// Iterator egyszer járható be
// sorok.toList -val listává alakítjuk, ha többször kell bejárni
//val sorok: Iterator[String] = tartalom.getLines()

val sorA = Right("5".toInt) //> Right[Nothing,Int] = Right(5)

// perc és bevétel 2 db konstans (val) kap értéket
"15 0.37".split(" ") //Array split eredménye //> res15: Array[String] = Array(15, 0.37)
val Array(perc, bevétel) = "15 0.37".split(" ") //> perc: String = 15, bevétel: String = 0.37
val sorB = Left((perc.toInt, bevétel.toDouble)) //> Left[(Int, Double),Nothing] = Left((15,0.37))

val boltokAbSoria = List(sorA, sorB) //> List[Either[(Int, Double),Int]] = List(Right(5), Left((15,0.37)))

val intek = List("4","5").map(str => str.toInt) //> List[Int] = List(4, 5)

val sorok = List("5", "15 0.37")
val boltokSoria = sorok.map { sor =>
  if (sor.contains(" ")) {
    val Array(perc, bevétel) = sor.split(" ")
    Left((perc.toInt, bevétel.toDouble))
  } else {
    Right(sor.toInt)
  }
}

következőSor(boltokSoria)

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
// match a Java switch helyett van

//      case Right(n) :: többiSor =>
// szétbontás történik a :: segítségével head :: tail formában,
// ahol head a 0. elem, tail pedig a további elemek listája
// n értéke a 0. elem mint Right-ból lesz kivéve

//     case Left((perc, bevétel)) :: többiSor =>
// perc és bevétel a 0. elem mint Left-ből lesz kiszedve