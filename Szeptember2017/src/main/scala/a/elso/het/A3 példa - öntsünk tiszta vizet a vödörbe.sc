
// Online változat: https://scalafiddle.io/sf/1t1UrlU/1


val összesVödörNagyságSzerint = List(11, 30, 47, 31, 32, 36, 3, 1, 5, 3, 32, 36, 15, 11, 46,
  26, 28, 1, 19, 3).sorted.reverse

val vízMennyisége = 150


def töltsd(vödrök: List[Int], következő: Int, eddigiMegoldás: List[Int]): List[Int] = {

  if (következő >= vödrök.size) {
    throw new Exception("Nem találtam megoldást")

  } else {

    val összŰrtartalom = eddigiMegoldás.sum + vödrök(következő)

    if (összŰrtartalom > vízMennyisége) {
      val kisebbVödör = következő + 1
      töltsd(vödrök, kisebbVödör, eddigiMegoldás)

    } else if (összŰrtartalom < vízMennyisége) {
      val újRészMegoldás = vödrök(következő) :: eddigiMegoldás
      val maradékVödrök = vödrök.diff(újRészMegoldás)
      töltsd(maradékVödrök, 0, újRészMegoldás)

    } else { // összŰrtartalom == vízMennyisége
      val megoldás = vödrök(következő) :: eddigiMegoldás
      megoldás

    }
  }
}


val megoldás: List[Int] = töltsd(összesVödörNagyságSzerint, 0, List())

println("Az 1. megtalált megoldás:")
println(megoldás)