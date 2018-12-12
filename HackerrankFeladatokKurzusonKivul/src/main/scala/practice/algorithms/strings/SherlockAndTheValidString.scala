package practice.algorithms.strings

import java.io.{FileReader, InputStreamReader, Reader, StringReader}

object SherlockAndTheValidString {
  //   object Solution {

  val testCase0 = "aabbcd" //NO
  val testCase1 = "aabbccddeefghi" //NO
  val testCase2 = "abcdefghhgfedecba" //YES
  val testCase6 = "xxxaabbccrry" //NO


  //noinspection VariablePatternShadow,ScalaUnnecessaryParentheses
  private def isValid(str: String) = {
    val groups = str.groupBy(c => c)
    val allLengths = groups.values.map(_.length)

    (
      allLengths.forall(_ == allLengths.head)

        ||

        groups.keys.exists { ch =>
          val lengths =
            groups.map { case (c, s) => if (c == ch) s.length - 1 else s.length }.
              filter { len => len > 0 }

          lengths.forall(_ == lengths.head)
        }
      )
  }

  def eval(reader: Reader): Unit = {
    scala.Console.withIn(reader) {
      val s = readLine()
      println(
        if (isValid(s)) "YES" else "NO"
      )
    }
  }

  def main(args: Array[String]) {

    // eval(new InputStreamReader(System.in)) /*

    println
    println("NO", testCase0)
    eval(new StringReader(testCase0))

    println
    println("NO", testCase1)
    eval(new StringReader(testCase1))

    println
    println("YES", testCase2)
    eval(new StringReader(testCase2))

    println
    println("NO", testCase6)
    eval(new StringReader(testCase6))

    val path = "HackerrankFeladatokKurzusonKivul/src/main/scala/practice/algorithms/strings/"

    println
    println("YES", "Test case 7.txt")
    eval(new FileReader(path + "Test case 7.txt"))

    println
    println("YES", "Test case 13.txt")
    eval(new FileReader(path + "Test case 13.txt"))

    println
    println("YES", "Test case 14.txt")
    eval(new FileReader(path + "Test case 14.txt"))

    // */
  }
}
