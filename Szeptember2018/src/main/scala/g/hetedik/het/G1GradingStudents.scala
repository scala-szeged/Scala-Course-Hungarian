package g.hetedik.het

import java.io.PrintWriter

// https://www.hackerrank.com/challenges/grading/problem

object Solution {

  /*
   * Complete the gradingStudents function below.
   */
  def gradingStudents(grades: Array[Int]): Array[Int] = {

    grades.map { g =>
      if (g < 38)
        g
      else if (g % 5 < 3)
        g
      else
        g / 5 * 5 + 5 // g / 5 eredménye Int, nem Double vagy Float
    }
  }

  def main(args: Array[String]) {
    println(
      gradingStudents(Array(73, 67, 38, 33)).toList
    )
    /*
        val scan = scala.Console

        val fw = new PrintWriter(sys.env("OUTPUT_PATH"))

        val n = scan.readLine.trim.toInt

        val grades = Array.ofDim[Int](n)

        for (gradesItr <- 0 until n) {
          val gradesItem = scan.readLine.trim.toInt
          grades(gradesItr) = gradesItem
        }

        val result = gradingStudents(grades)

        fw.println(result.mkString("\n"))

        fw.close()
    */
  }
}
