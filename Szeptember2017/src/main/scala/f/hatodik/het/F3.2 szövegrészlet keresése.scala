import scala.io.Source

object SzövegrészletKeresése {

  def main(args: Array[String]): Unit = {
    val fájl = Source.fromFile("Szeptember2017/src/main/scala/c/harmadik/het/csomagolópapír.txt")
    val szöveg = fájl.toList

    println("részListaE(szöveg, \"10x22\".toList): " + részListaE(szöveg, "10x22".toList))
    println("részListaE(szöveg, \"kémia\".toList): " + részListaE(szöveg, "kémia".toList))
    println("részListaE(szöveg, \"251x173\".toList): " + részListaE(szöveg, "251x173".toList))
  }

  def részListaE[A](aElőlről: List[A], bElőlről: List[A]): Boolean = {

    def loop(aLista: List[A], bLista: List[A]): Boolean =
      (aLista, bLista) match {

        case (a :: _, b :: Nil) if a == b =>
          true

        case (_, Nil) =>
          true

        case (Nil, _) =>
          false

        case (a :: aTovább, b :: bTovább) =>
          if (a == b) {
            loop(aTovább, bTovább)
          } else if (a == bElőlről.head) {
            loop(a :: aTovább, bElőlről)
          } else {
            loop(aTovább, bElőlről)
          }
      }

    loop(aElőlről, bElőlről)
  }
}

/*
részListaE(
  """
    |19x21x30
    |17x26x12
    |7x5x10
    |2x22x14
    |10x17x30
    |1x8x5
    |23x2x25
    |22x29x8
    |13x26x1
    |26x3x30
    |25x17x8
    |25x18x26
    |26x19x15
    |8x28x10
    |12x16x29
    |30x6x29
    |28x19x4
    |27x26x18
    |15x23x17
    |5x21x30
    |8x11x13
    |2x26x7
    |19x9x24
    |3x22x23
    |6x7x18
    |4x26x30
    |13x25x20
    |17x3x15
    |8x20x18
    |23x18x23
    |28x23x9
    |16x3x4
    |1x29x14
    |20x26x22
    |3x2x22
    |23x8x17
    |19x5x17
    |21x18x20
    |17x21x8
    |30x28x1
    |29x19x23
    |12x12x11
    |24x18x7
    |21x18x14
    |14x26x25
    |9x11x3
    |10x7x15
    |27x6x28
    |14x26x4
    |28x4x1
    |22x25x29
    |6x26x6
    |1x3x13
    |26x22x12
    |6x21x26
    |23x4x27
    |26x13x24
    |5x24x28
    |22x16x7
    |3x27x24
    |19x28x2
    |11x13x9
    |29x16x22
    |30x10x24
    |14x14x22
    |22x23x16
    |14x8x3
    |20x5x14
    |28x6x13
    |3x15x25
    |4x12x22
    |15x12x25
    |10x11x24
    |7x7x6
    |8x11x9
    |21x10x29
    |23x28x30
    |8x29x26
    |16x27x11
    |1x10x2
    |24x20x16
    |7x12x28
    |28x8x20
    |14x10x30
    |1x19x6
    |4x12x20
    |18x2x7
    |24x18x17
    |16x11x10
    |1x12x22
    |30x16x28
    |18x12x11
    |28x9x8
    |23x6x17
    |10x3x11
    |5x12x8
    |22x2x23
    |9x19x14
    |15x28x13
    |27x20x23
    |19x16x12
    |19x30x15
    |8x17x4
    |10x22x18
    |13x22x4
    |3x12x19
    |22x16x23
    |11x8x19
    |8x11x6
    |7x14x7
    |29x17x29
    |21x8x12
    |21x9x11
    |20x1x27
    |1x22x11
    |5x28x4
    |26x7x26
    |30x12x18
    |29x11x20
    |3x12x15
    |24x25x17
    |14x6x11
""".toList, "251x173".toList)
*/