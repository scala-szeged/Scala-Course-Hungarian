
// http://www.scala-sbt.org/0.13/docs/Configuring-Scala.html
// kell az import -hoz, hogy ne legyen piros,
// viszont úgy is működik, mert Scala Workshhet (sc) fájlban van

import scala.reflect.runtime.universe._

val a = Option(1)
val b = Option(2)
val c = Option(3)

val valódiKifejezése =
  show { reify {
    for { i <- a ; j <- b ; k <- c } yield i + j + k
  }}

/*
valódiKifejezése: String = Expr[scala.Option[Int]](

a.flatMap(i =>
  b.flatMap(j =>
    c.map(k => i.$plus(j).$plus(k))
  )
)
*/
