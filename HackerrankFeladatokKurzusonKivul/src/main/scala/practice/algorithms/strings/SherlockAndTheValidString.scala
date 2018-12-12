package practice.algorithms.strings

import java.io.{FileReader, InputStreamReader}

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

  def eval(reader: InputStreamReader): Unit = {
    scala.Console.withIn(reader) {
      val s = readLine()
      println(
        if (isValid(s)) "YES" else "NO"
      )
    }
  }

  def main(args: Array[String]) {

    // eval(new InputStreamReader(System.in)) /*

    println(isValid(testCase0), "NO", testCase0)
    println(isValid(testCase1), "NO", testCase1)
    println(isValid(testCase2), "YES", testCase2)
    println(isValid(testCase6), "NO", testCase6)

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
