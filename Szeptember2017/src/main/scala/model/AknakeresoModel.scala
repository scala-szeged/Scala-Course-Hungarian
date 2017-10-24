package model

object AknakeresoModel {

  class Cella

  case object Akna extends Cella

  case class Szám(n: Int) extends Cella

  case object TakartAkna extends Cella

  case class TakartSzám(n: Int) extends Cella


  type Tábla = List[List[Cella]]
}