
def render(xs: Any*) = println(xs)

def open = scala.sys.process.Process(Seq("open", "diagram.png")).run()

// Viszkok Tamás megoldása

def töltsd(víz: Int, vödrök: List[Int], részMegoldás: List[Int]): List[List[Int]] = {

  if (vödrök != Nil) {

    val újMegoldás = vödrök.head :: részMegoldás

    if (újMegoldás.sum == víz) {
      render(újMegoldás, vödrök.tail, részMegoldás); open; readLine
      újMegoldás :: töltsd(víz, vödrök.tail, részMegoldás)

    } else if (újMegoldás.sum < víz) {
      render(vödrök.tail, újMegoldás, részMegoldás); open; readLine
      töltsd(víz, vödrök.tail, újMegoldás) ::: töltsd(víz, vödrök.tail, részMegoldás)

    } else {
      render(vödrök.tail, részMegoldás); open; readLine
      töltsd(víz, vödrök.tail, részMegoldás)
    }

  } else {
    Nil
  }
}

val összesVödör = 5 :: 20 :: 25 :: 30 :: 100 :: Nil

val vízMennyisége = 150

val összesVödörNagyságSzerint = összesVödör.sorted.reverse
println("--- Összes vödör nagyság szerint")
println(összesVödörNagyságSzerint)

val indulás = System.currentTimeMillis()
val megoldások = töltsd(vízMennyisége, összesVödörNagyságSzerint, Nil)
val befejezés = System.currentTimeMillis()

println("--- Megoldások száma")
println(megoldások.size)
println("--- Megoldások")
megoldások.foreach(println)
println("--- Számítási idő (milliszekundum)")
println(befejezés - indulás)





összesVödör.combinations(3).toList.filter(_.sum == 150)
összesVödör.combinations(4).toList.filter(_.sum == 150)

val list = for (i <- (4 to 4))
  yield összesVödör.combinations(i).toList.filter(
    _.sum == 150
  )
list
