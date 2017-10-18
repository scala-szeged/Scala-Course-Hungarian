
// --- implicit

// Ebben a példában a contextus egyes beállításait adja meg

object G_1_2_ÚjIsmeret extends App {

  object A {
    implicit val a: String = "a"
  }

  object B {
    implicit val b: String = "b"
  }


  def print(n: Int)(implicit s: String): Unit = {
    println(n, s)
  }

  def print1(): Unit = {

    import A._

    print(1)
  }

  def print2(): Unit = {

    import B._

    print(2)
  }


  print1()
  print2()
}
