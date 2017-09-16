
// --- case class -ok

case class Személy(név: String, kor: Int)

val ádám = Személy("Ádám", 42)



// A case class -okkal ezeket is kapjuk:
// hashcode, equals, toString
// plusz serializable lesz az objektumunk




// Nem végrehajtható: Reassignment to val
// ádám.név = "Éva"



//  Mivel a mezõk nem írhatóak helyette ezt kell csinálni:

ádám.copy(név = "Éva")
