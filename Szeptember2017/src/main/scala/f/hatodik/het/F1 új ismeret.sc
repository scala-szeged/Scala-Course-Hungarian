

val listaNullákkalfeltöltve = List.fill(7)(0)

val listaEgyesekkelfeltöltve = List.fill(5)(1)


val sz = "szöveg"
val karakterekListája = sz.toList
val karakterekListája2 = "további szöveg".toList


// val fájl =scala.io.Source.fromFile("csomagolópapír.txt")
// val karakterekListája3 = fájl.toList


// az   @annotation.tailrec ugyanazt az eredményt hozza, mint az
// import scala.annotation.tailrec
// és az után a
// @tailrec használata az adott metóduson
// Fordítási hibát okoz, ha az adott metódus nem végrekurzív



// --- A Trampoline /Suspend / Return nem része a számonkérésnek