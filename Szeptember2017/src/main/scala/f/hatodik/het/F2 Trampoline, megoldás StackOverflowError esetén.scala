package f.hatodik.het

object F2Trampoline {

  def main(args: Array[String]): Unit = {

    val ints = List.fill(10000)(0)

    println(nemVégRekurzív(ints))
  }

  def nemVégRekurzív[A](list: List[A]): Int = list match {
    case Nil =>
      0

    case _ :: listaVége =>
      nemVégRekurzív(listaVége) + 1
  }
}
