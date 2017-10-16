package f.hatodik.het

object F2Trampoline {

  def main(args: Array[String]): Unit = {

    val ints = List.fill(10000)(0)

    // nem végrekurzív a metódus, így nem iterációként, hanem rekurzióként fordítja le a scala fordító
    // végrehajtáskor a stack méretnél nagyobb lista méret StackOverflowError -t okoz
    println(nemVégRekurzív(ints))
  }

  def nemVégRekurzív[A](list: List[A]): Int = list match {
    case Nil =>
      0

    case _ :: listaVége =>
      nemVégRekurzív(listaVége) + 1
  }
}

/*
Aább található a megoldás, amit a https://github.com/kenbot/free projektbe belehelyezve lehet kipróbálni
és stack helyett heap -et használ:

def nemVégRekurzív[A](list: List[A]): Trampoline[Int] = list match {
    case Nil =>
      Return(0)

    case _ :: listaVége =>
      Suspend(() => nemVégRekurzív(listaVége)).flatMap { n =>
        Return(n + 1)
      }
  }
*/