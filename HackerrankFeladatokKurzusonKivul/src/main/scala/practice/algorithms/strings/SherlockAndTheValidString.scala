package practice.algorithms.strings

import java.io.PrintWriter

object SherlockAndTheValidString {
  //   object Solution {

  val sampleInput0 = "aabbcd" //NO
  val sampleInput1 = "aabbccddeefghi" //NO
  val sampleInput2 = "abcdefghhgfedecba" //YES


  //noinspection VariablePatternShadow
  private def isValid(str: String) = {
    val groups = str.groupBy(c => c)
    val ((max, maxCount), (min, minCount)) = groups.foldLeft(((Int.MinValue, 0), (Int.MaxValue, 0))) {
      case (((max, maxCount), (min, minCount)), (c, s)) =>
        val len = s.length
        (
          if (len > max) (len, 1) else if (len == max) (max, maxCount + 1) else (max, maxCount),
          if (len < min) (len, 1) else if (len == min) (min, minCount + 1) else (min, minCount)
        )
    }

    max == min ||
      (max == min + 1 &&
        (maxCount == 1 || minCount == 1))
  }

  def main(args: Array[String]) {
    println(sampleInput0, isValid(sampleInput0))
    println(sampleInput1, isValid(sampleInput1))
    println(sampleInput2, isValid(sampleInput2))

    /*
        val stdin = scala.Console
        val printWriter = new PrintWriter(sys.env("OUTPUT_PATH"))
        val s = stdin.readLine
        val result = isValid(s)
        printWriter.println(result)
        printWriter.close()
    */
  }
}
