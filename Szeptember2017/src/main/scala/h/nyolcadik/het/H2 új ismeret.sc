
// --- flatMap

// flatMap -et, használunk map helyett
// amikor 1-1 input elemhez több output elem tartozik

List("a b").flatMap(_.split(" "))
List("a b").map(_.split(" "))

"a b".split(" ").getClass



// --- zipWithIndex újra

for {(sor, sorIndex) <- List(6,7,8).zipWithIndex} yield (sor,sorIndex)



// --- Set adatszerkezet

val intSet = Set(3,4,5)
val intSetben3 = intSet(3) // A Set tartatlmazza-e a 3 -as számot?
val intSetben7 = intSet(7) // A Set tartatlmazza-e a 7 -as számot?

val tupleSet = Set((1,2),(2,4),(3,6))
val tupleSetben24 = tupleSet(2,4) // A Set tartatlmazza-e a (2,4) Tupple -t?
val tupleSet24nélkül = tupleSet - Tuple2(2,4) // A Set tartatlmazza-e a 3 -as számot?


