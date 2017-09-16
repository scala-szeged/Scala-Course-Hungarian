import data.Person


// --- Metódusok, illetve függvények

def isMajor1(p: Person): Boolean = {p.age >= 18} // jó
def isMajor2(p: Person): Boolean = p.age >= 18 // jó
def isMajor3(p: Person) = p.age >= 18 // jó
def isMajor4(p: Person) = {p.age >= 18} // jó
def isMajor5(p: Person) {p.age >= 18} // nem jó


def makeItDouble(x: Int): Int = {
  val y = 2 * x
  println(y)
  y // Az utolső kifejezés a visszatérési érték
}


makeItDouble(5)
