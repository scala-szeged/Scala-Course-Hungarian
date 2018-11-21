package k.tizenegyedik.het

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

// todo a tanultakkal megoldani, https://www.hackerrank.com/challenges/filter-elements/problem

object B3__Solution {
  def main(args: Array[String]): Unit = {
    val scan = scala.Console

    val t = scan.readLine.toInt
    for (_ <- 1 to t) {
      val Array(n, k) = scan.readLine.split(" ")
      val line = scan.readLine
      szűr(line, k.toInt)
    }
  }

  def szűr(line: String, k: Int): Unit = {
    val array = line.split(" ")

    val eredmény = groupByInInsertionOrder(array).filter {

      case (elem, lista) => lista.size >= k
    }.map {
      case (elem, lista) => elem
    }

    if (eredmény.nonEmpty)
      println(eredmény.mkString(" "))
    else
      println(-1)
  }

  def groupByInInsertionOrder[A](seq: Seq[A]): mutable.LinkedHashMap[A, ArrayBuffer[A]] = {
    val m = mutable.LinkedHashMap.empty[A, ArrayBuffer[A]]
    for (elem <- seq) {
      val bldr = m.getOrElseUpdate(elem, ArrayBuffer[A]())
      bldr.append(elem)
    }
    m
  }
}
