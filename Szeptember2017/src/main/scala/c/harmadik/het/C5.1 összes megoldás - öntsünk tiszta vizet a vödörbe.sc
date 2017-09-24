


def töltsd(víz: Int, vödrök: List[Int], részMegoldás: List[Int], írdFel: List[Int] => Unit): Unit = {

  if (vödrök != Nil) {

    val újMegoldás = vödrök.head :: részMegoldás

    if (újMegoldás.sum == víz) {
      írdFel(újMegoldás)

    } else if (újMegoldás.sum < víz) {
      töltsd(víz, vödrök.tail, újMegoldás, írdFel)

    }

    töltsd(víz, vödrök.tail, részMegoldás, írdFel)
  }
}

val összesVödör = 11 :: 30 :: 47 :: 31 :: 32 :: 36 :: 3 :: 1 :: 5 ::
  3 :: 32 :: 36 :: 15 :: 11 :: 46 :: 26 :: 28 :: 1 :: 19 :: 3 :: Nil

val vízMennyisége = 150

val összesVödörNagyságSzerint = összesVödör.sorted.reverse
println("--- Összes vödör nagyság szerint")
println(összesVödörNagyságSzerint)

var megoldások = Nil: List[List[Int]]
val írdFel = { megoldás: List[Int] => megoldások = megoldás :: megoldások }

val indulás = System.currentTimeMillis()
töltsd(vízMennyisége, összesVödörNagyságSzerint, Nil, írdFel)
val befejezés = System.currentTimeMillis()

println("--- Megoldások")
megoldások.foreach(println)
println("--- Megoldások száma")
println(megoldások.size)
println("--- Számítási idő (milliszekundum)")
println(befejezés - indulás)