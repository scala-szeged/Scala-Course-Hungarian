package g.hetedik.het

import java.io.PrintWriter

// https://www.hackerrank.com/challenges/grading/problem

object Solution {

  /*
   * Complete the gradingStudents function below.
   */
  def gradingStudents(grades: Array[Int]): Array[Int] = {

    grades
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
