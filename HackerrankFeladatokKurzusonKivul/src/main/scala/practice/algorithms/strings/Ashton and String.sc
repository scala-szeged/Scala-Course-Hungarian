
// https://www.hackerrank.com/challenges/ashton-and-string

// More elegant solution, but also times out: https://scalafiddle.io/sf/A0Dbtlu/0?fbclid=IwAR2usxCAZLYo2Obh_O5nQoovIulCF9vcY-7q-C7bGtvlS0VO-Ub6wRkQtbs

class Slice(i: Int, j: Int, s: String) {

  def charAtSlice(n: Int) = s.charAt(i + n)

  def lengthOfSlice = j - i

  //noinspection RemoveRedundantReturn
  def compareTo(other: Slice): Int = {
    val len1 = j - i
    val len2 = other.j - other.i
    val lim = Math.min(len1, len2)

    for (k <- 0 until lim) {
      val c1 = s.charAt(i + k)
      val c2 = other.s.charAt(other.i + k)
      if (c1 != c2) {
        return c1 - c2
      }
    }

    return len1 - len2
  }

  override def toString: String = s.slice(i, j)
}

def ashtonString(s: String, k: Int) = {
  val len = s.length

  val arr = for {
    i <- 0 to len - 1
    j <- i + 1 to len
  } yield new Slice(i, j, s)


  trait SliceOrderingT extends Ordering[Slice] {
    def compare(x: Slice, y: Slice) = {
      x.compareTo(y)
    }
  }
  implicit object SliceOrdering extends SliceOrderingT

  arr.distinct.sorted.foldLeft((k - 1, '-')) {
    case ((-1, c), slice) =>
      //println(c)
      (-1, c)

    case ((n, c), slice) =>
      val len = slice.lengthOfSlice
      if (len >= n) (-1, slice.charAtSlice(n)) else (n - len, c)
  }._2
}

//ashtonString("pfpgrnlorzzhdoxzsnemubzvkcbbfb", 77)
//val expected = "o"
ashtonString("abcd", 5)
val expected0 = "b"

ashtonString("dbac", 3)
val expected1 = "c"



def fibs: Stream[BigInt] = BigInt(0) #:: BigInt(1) #:: fibs.zip(fibs.tail).map { n => n._1 + n._2 }

fibs take 5 foreach println


val testCase1 =
  """
    |5
    |nvzjkcahjwlhmdiuobjdwbanmvrtadopapbktdtezellktgywrdstdhhayaadqrdhspavjgxprk
    |2071
    |szkkcedhlkhjnjofbrnvhntsushncjavkmfn
    |465
    |wcweojncpqwcofrhxnzgbdrd
    |251
    |pfpgrnlorzzhdoxzsnemubzvkcbbfb
    |77
    |judaioobpoiteiszvzlscmpmpqqwuvtdqzdapudfimaowsnttalwndievaapwusmtyoksrpcfpqbkgvfiibvlkbjkcy
    |2473""".stripMargin.lines.toList.tail.tail

val testCase1Out = "b d d o w" //.split(" ").mkString

val results = for (s :: k :: Nil <- testCase1.grouped(2)) yield ashtonString(s, k.toInt)
results.toList

// Test case 3 Terminated due to timeout
// Test case 5 Runtime Error

