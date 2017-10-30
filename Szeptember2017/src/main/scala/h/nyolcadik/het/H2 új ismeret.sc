
// --- foldLeft

// def foldLeft[A](a: A)(f0: (A, T) => A): A
// A - ez a kezdő érték és az eredmény típusa
// f0 - függvény (metódus), ami kettő paraméterű:
// az 1. paraméter az aktuális eredmény, ami először a kezdőérték
// a 2. paraméter a soronkövetkező elem
// az általa kiszámolt eredmény a következő f0 híváskor az 1. paraméter lesz

// Így például megszámolhatjuk, hogy egy listának hány eleme van
List(1, 2, 3).foldLeft(0) { (a, x) =>
  a + 1
}

// vagy kiszámolhatjuk az összes elem szorzatzát
List(1, 2, 3).foldLeft(1) { (a, x) =>
  a * x
}



// --- flatMap

// flatMap -et, használunk map helyett
// amikor 1-1 input elemhez több output elem tartozik

List("a b").flatMap(_.split(" "))
List("a b").map(_.split(" "))

"a b".split(" ").getClass



// --- zipWithIndex újra

for {(elem, index) <- List(6, 7, 8).zipWithIndex} yield index



// --- Set adatszerkezet

val intSet = Set(3, 4, 5)
val intSetben3 = intSet(3) // A Set tartatlmazza-e a 3 -as számot?
val intSetben7 = intSet(7) // A Set tartatlmazza-e a 7 -as számot?

val tupleSet = Set((1, 2), (2, 4), (3, 6))
val tupleSetben24 = tupleSet(2, 4) // A Set tartatlmazza-e a (2,4) Tupple -t?
val tupleSet24nélkül = tupleSet - Tuple2(2, 4)


