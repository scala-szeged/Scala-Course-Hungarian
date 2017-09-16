
// --- Import teljes package -re

import scala.io._



// --- Közönséges osztályok

class A(age: Int)
val a = new A(10)

// Mem elérhetõ az age mezõ, ezért hibajelzést kapnánk:
// a.age



class B(val name: String) {
  override def toString = s"B($name)"
}

val b = new B("Dezsõ")

// A name mező elérhetõ:
b.name
