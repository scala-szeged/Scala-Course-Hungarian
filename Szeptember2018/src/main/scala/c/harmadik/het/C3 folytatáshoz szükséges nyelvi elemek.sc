
List(1, 2, 3, 4, 5).drop(2) //> res6: = List(3, 4, 5)
List(1, 2, 3, 4, 5).take(2) //> res7: = List(1, 2)
List(1, 2, 3, 4, 5).slice(2, 4) //> res8: = List(3, 4)

List("a", "b", "c").indices //> res9: = Range 0 until 3
List("a", "b", "c").indices.last //> res10: Int = 2
List("a", "b", "c").last //> res11: String = c

// Touple 2 vagy több elemű
("a", "b") //> res12: (String, String) = (a,b)
("a", "b")._1 //> res13: String = a
("a", "b")._2 //> res14: String = b