
for {
  c <- 1 to 10 // c-be kerüljenek bele a számok 1-től 10-ig
  if c > 8 // szűrés
}
  yield c // az előállított collection egyik eleme



// Másként formázva

for (c <- 1 to 10 if c > 8) yield c
