
// --- Magasabb szintű típus

// az alábbi Olvasó nevű trait (interfész) P[_] típus paramétere
// magasabb színtű típus, illetve típus konstruktor

trait Olvasó[P[_]] {
  def olvasd(): P[Int]
}

// List is egy típus konstruktor, mert pl az Int típust kapva
// konstruktor paraméterként a List[Int] típust adja vissza

object ListaOlvasó extends Olvasó[List] {
  def olvasd(): List[Int] = List(4, 2, 5, 6)
}

object OpcióOlvasó extends Olvasó[Option] {
  def olvasd(): Option[Int] = Some(42)
}

val listaOlvasó: Olvasó[List] = ListaOlvasó
listaOlvasó.olvasd()

val opcióOlvasó: Olvasó[Option] = OpcióOlvasó
opcióOlvasó.olvasd()



// Ez egy type alias, ami magasabb színtű típus -ként használhatóvá
// teszi Now -t. A jelentése pedig az általa bebúrkolt típus (X) lesz

type Now[X] = X



// http://www.scala-sbt.org/0.13/docs/Configuring-Scala.html
// kell az import -hoz, hogy ne legyen piros,
// viszont úgy is működik, mert Scala Workshhet (sc) fájlban van

import scala.reflect.runtime.universe._

val a = Option(1)
val b = Option(2)
val c = Option(3)

val erreFordítjaAFordító =
  show {
    reify {
      for {i <- a; j <- b; k <- c} yield i + j + k
    }
  }

/*
erreFordítjaAFordító: String = Expr[scala.Option[Int]](

a.flatMap(i =>
  b.flatMap(j =>
    c.map(k => i.$plus(j).$plus(k))
  )
)
*/
