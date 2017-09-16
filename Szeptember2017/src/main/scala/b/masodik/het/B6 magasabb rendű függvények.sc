import data.Person


// --- Magasabb rendű függvények


// Ezek függvény konstansok

val isMajorUs: Int => Boolean =   age => age >= 21
val isMajorHu: Int => Boolean = { age => age >= 18 }


// Ez a magaabb rendű függvény, mert van olyan paramétere,
// ami maga is függvény. A predicate nevű paraméter az

def isMajorPerson(p: Person, predicate: Int => Boolean): Boolean = {
  predicate(p.age)
}


// Akkor is magasabb rendű függvény, ha a
// visszatérési értéke függvény


// Így használjuk

val isJohnMajor = isMajorPerson(Person("John", 18), isMajorUs)
val jánosNagykorúE = isMajorPerson(Person("János", 18), isMajorHu)
