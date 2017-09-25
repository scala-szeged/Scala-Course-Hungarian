
// Viszkok Tamás megoldása

def töltsd(víz: Int, vödrök: List[Int], részMegoldás: List[Int]): List[List[Int]] = {

  if (vödrök != Nil) {

    val újMegoldás = vödrök.head :: részMegoldás

    if (újMegoldás.sum == víz) {
      újMegoldás :: töltsd(víz, vödrök.tail, részMegoldás)

    } else if(újMegoldás.sum < víz){
      töltsd(víz, vödrök.tail, újMegoldás) ::: töltsd(víz, vödrök.tail, részMegoldás)

    } else {
      töltsd(víz, vödrök.tail, részMegoldás)
    }

  } else {
    Nil
  }
}

val összesVödör = 11 :: 30 :: 47 :: 31 :: 32 :: 36 :: 3 :: 1 :: 5 ::
  3 :: 32 :: 36 :: 15 :: 11 :: 46 :: 26 :: 28 :: 1 :: 19 :: 3 :: Nil

val vízMennyisége = 150

val összesVödörNagyságSzerint = összesVödör.sorted.reverse
println("--- Összes vödör nagyság szerint")
println(összesVödörNagyságSzerint)

val indulás = System.currentTimeMillis()
val megoldások = töltsd(vízMennyisége, összesVödörNagyságSzerint, Nil)
val befejezés = System.currentTimeMillis()

println("--- Megoldások")
megoldások.foreach(println)
println("--- Megoldások száma")
println(megoldások.size)
println("--- Számítási idő (milliszekundum)")
println(befejezés - indulás)