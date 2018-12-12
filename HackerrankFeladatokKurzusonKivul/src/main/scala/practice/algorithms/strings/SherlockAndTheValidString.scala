package practice.algorithms.strings

import java.io.{FileReader, InputStreamReader}

object SherlockAndTheValidString {
  //   object Solution {

  val testCase0 = "aabbcd" //NO
  val testCase1 = "aabbccddeefghi" //NO
  val testCase2 = "abcdefghhgfedecba" //YES
  val testCase6 = "xxxaabbccrry" //NO


  //noinspection VariablePatternShadow
  private def isValid(str: String) = {
    val groups = str.groupBy(c => c)
    val ((max, maxCount), (min, minCount)) =
      groups.foldLeft(((Int.MinValue, 0), (Int.MaxValue, 0))) {
        case (((max, maxCount), (min, minCount)), (c, s)) =>
          val len = s.length
          (
            if (len > max) (len, 1) else if (len == max) (max, maxCount + 1) else (max, maxCount),
            if (len < min) (len, 1) else if (len == min) (min, minCount + 1) else (min, minCount)
          )
      }


    groups.forall { case (c, s) => s.length == 1 } ||
      (groups.count { case (c, s) => s.length == 1 } == 1 &&
        groups.filter { case (c, s) => s.length > 1 }.forall { case (c, s) => s.length == groups.head._2.length }) ||
      (max == min + 1 && maxCount == 1)
  }

  def eval(reader: InputStreamReader): Unit = {
    scala.Console.withIn(reader) {
      val s = readLine
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

    // */
  }
}
