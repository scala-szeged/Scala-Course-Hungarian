
def darabold(vasSzál: Int, rudak: List[Int], eddigiMegoldás: List[Int],
             írdFel: List[Int] => Unit): Unit = {

  if (rudak != List()) {

    val újMegoldás = rudak.head :: eddigiMegoldás

    if (újMegoldás.sum == vasSzál) {
      írdFel(újMegoldás)

    } else if (újMegoldás.sum < vasSzál) {
      darabold(vasSzál, rudak.tail, újMegoldás, írdFel)

    }

    darabold(vasSzál, rudak.tail, eddigiMegoldás, írdFel)
  }
}

// A méretek legyenek mondjuk inch-ben

val vasRudak = List(11, 30, 47, 31, 32, 36, 3, 1, 5, 3, 32,
  36, 15, 11, 46, 26, 28, 1, 19, 3)

val vasSzálHossza = 150

val összesVasRúdNagyságSzerint = vasRudak.sorted.reverse
println("--- Összes VasRúd nagyság szerint")
println(összesVasRúdNagyságSzerint)

var megoldások = List(): List[List[Int]]
def írdFel(megoldás: List[Int]): Unit = {
  megoldások = megoldás :: megoldások
}

val indulás = System.currentTimeMillis()
darabold(vasSzálHossza, összesVasRúdNagyságSzerint, List(), írdFel)
val befejezés = System.currentTimeMillis()

println("--- Megoldások")
megoldások.foreach(println)
println("--- Megoldások száma")
println(megoldások.size)
println("--- Számítási idő (milliszekundum)")
println(befejezés - indulás)