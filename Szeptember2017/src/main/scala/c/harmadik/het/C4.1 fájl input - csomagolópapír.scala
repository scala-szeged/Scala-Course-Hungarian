package c.harmadik.het

import scala.io.Source


// --- Fájl input


object Csomagolópapír1 extends App {

  val fájl = Source.fromFile("Szeptember2017/src/main/scala/c/harmadik/het/csomagolópapír.txt")
  val str = fájl.mkString
  val iterator = str.lines
  val dobozok = iterator.toList

  // szélesség, magasság és hosszúság
  def szmh(doboz:String): Array[Int] = doboz.split('x').map(str => str.toInt)

  val eredmény = dobozok.map(doboz => szmh(doboz) match {
    case Array(sz, m, h) =>
      val területek = List(sz * m, sz * h, m * h)
      val legkisebb = területek.min
      val papírEgyDobozra = legkisebb + 2 * területek.sum
      papírEgyDobozra
  }).sum

  println("A szükséges csomagolópapír mennyisége: " + eredmény)
}