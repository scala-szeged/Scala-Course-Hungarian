val számok1 = for { // konstans definiálása (immutable)
  c <- 1 to 10 // c-be kerüljenek bele a számok 1-től 10-ig
  if c > 8 // szűrés
}
  yield c // az előállított collection egyik eleme

println(számok1)


// Másként formázva

val számok2 = for (c <- 10 to 20 if c > 17) yield c
println(számok2)
