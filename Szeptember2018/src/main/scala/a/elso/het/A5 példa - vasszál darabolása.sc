
val összesVasRúdNagyságSzerint =
  List(11, 30, 47, 31, 32, 36, 3, 1, 5, 3, 32, 36, 15, 11, 46,
  26, 28, 1, 19, 3).sorted.reverse // cm

val vasSzálHossza = 150 // cm


def darabold(vasRudak: List[Int], következő: Int, eddigiMegoldás: List[Int]): List[Int] = {

  if (következő >= vasRudak.size) {
    throw new Exception("Nem találtam megoldást")

  } else {

    val összHossz = eddigiMegoldás.sum + vasRudak(következő)

    if (összHossz > vasSzálHossza) {
      val kisebbVasRúd = következő + 1
      darabold(vasRudak, kisebbVasRúd, eddigiMegoldás)

    } else if (összHossz < vasSzálHossza) {
      val újRészMegoldás = vasRudak(következő) :: eddigiMegoldás
      val maradékVasRudak = vasRudak.diff(újRészMegoldás)
      darabold(maradékVasRudak, 0, újRészMegoldás)

    } else { // összHossz == vasSzálHossza
      val megoldás = vasRudak(következő) :: eddigiMegoldás
      megoldás

    }
  }
}


val megoldás: List[Int] = darabold(összesVasRúdNagyságSzerint, 0, List())

println("Az 1. megtalált megoldás:")
println(megoldás)