package model

object AknakeresoModel {

  class Cella

  class TakartCella extends Cella

  case object Akna extends Cella

  case class Szám(n: Int) extends Cella

  case object TakartAkna extends TakartCella

  case class TakartSzám(n: Int) extends TakartCella


  type Tábla = List[List[Cella]]
}