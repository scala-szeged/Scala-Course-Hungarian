
// http://www.scala-sbt.org/0.13/docs/Configuring-Scala.html
// ez kell az import -hoz, viszont úgy is működik, hogy piros:
import scala.reflect.runtime.universe._

val a, b, c = Option(1)
show { reify {
  for { i <- a ; j <- b ; k <- c } yield (i + j + k)
}}
