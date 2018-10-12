package i.tizenharmadik.het

// https://open.kattis.com/problems/textprocessor
object I1 {


  def main(args: Array[String]): Unit = {
    val scan = scala.Console
    val szó = scan.readLine

    val (q, w) = scan.readf2("{0} {1}")

    for (_ <- 1 to q.asInstanceOf[String].toInt) {
      val i = scan.readInt()
      val darab = szó.slice(i - 1, i - 1 + w.asInstanceOf[String].toInt)
      //println(darab)
      process(darab)
    }
  }

  def process(szó: String) {
    //if (szó.toList.toSet.size == szó.length)
    //  {vmi optimálisabb}

    val eredmény = (1 until szó.length).foldLeft(1) {
      case (n, i) =>
        val készlet = szó.sliding(i).toSet
        n + készlet.size
    }

    println(eredmény)
  }
}
