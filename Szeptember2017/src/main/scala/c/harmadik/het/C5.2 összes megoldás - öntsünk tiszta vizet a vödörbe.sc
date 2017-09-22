def töltsd(víz: Int, vödrök: List[Int], részMegoldás: List[Int], megoldások: List[List[Int]]): List[List[Int]] = {

  if (vödrök != Nil) {

    val újMegoldás = vödrök.head :: részMegoldás

    if (újMegoldás.sum == víz) {
      újMegoldás::megoldások ::: töltsd(víz, vödrök.tail, részMegoldás, megoldások)
    } else if(újMegoldás.sum < víz){
      töltsd(víz, vödrök.tail, újMegoldás, megoldások) ::: töltsd(víz, vödrök.tail, részMegoldás, megoldások)
    } else {
      töltsd(víz, vödrök.tail, részMegoldás, megoldások)
    }
  } else {
    megoldások
  }
}

val összesVödör = 11 :: 30 :: 47 :: 31 :: 32 :: 36 :: 3 :: 1 :: 5 ::
  3 :: 32 :: 36 :: 15 :: 11 :: 46 :: 26 :: 28 :: 1 :: 19 :: 3 :: Nil

val vízMennyisége = 150

val összesVödörNagyságSzerint = összesVödör.sorted.reverse
println("--- Összes vödör nagyság szerint")
println(összesVödörNagyságSzerint)

var megoldások = Nil: List[List[Int]]

val indulás = System.currentTimeMillis()
val megoldas = töltsd(vízMennyisége, összesVödörNagyságSzerint, Nil, Nil)
val befejezés = System.currentTimeMillis()

println("--- Megoldások")
megoldások.foreach(println)
println("--- Megoldások száma")
println(megoldas.size)
println("--- Számítási idő (milliszekundum)")
println(befejezés - indulás)