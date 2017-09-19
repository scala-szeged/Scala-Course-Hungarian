package c.harmadik.het

import scala.io.Source

object Csomagolópapír2 extends App {

  val fájl = Source.fromFile("Szeptember2017/src/main/scala/c/harmadik/het/csomagolópapír.txt")
  val str = fájl.mkString
  val iterator = str.lines
  val dobozok = iterator.toList

  // szélesség, magasság és hosszúság
  val szmh = """(\d+)x(\d+)x(\d+)""".r

  val eredmény = dobozok.map(doboz => doboz match {
    case szmh(szStr, mStr, hStr) =>
      val sz = szStr.toInt
      val m = mStr.toInt
      val h = hStr.toInt
      val területek = List(sz * m, sz * h, m * h)
      val legkisebb = területek.min
      val papírEgyDobozra = legkisebb + 2 * területek.sum
      papírEgyDobozra
  }).sum

  println("A szükséges csomagolópapír mennyisége: " + eredmény)
}