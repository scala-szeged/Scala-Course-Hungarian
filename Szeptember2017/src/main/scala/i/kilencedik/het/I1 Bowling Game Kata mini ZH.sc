
List(1, 2, 3).init
List(1, 2, 3).last

List(1, 2, 3, 4, 5).sliding(3).toList


def végNullák(gurítások: List[Int]) =
  (gurítások.init.last, gurítások.last) match {
    case (y, z) => gurítások
  }

def eredmény(gurítások: List[Int]) =
  végNullák(gurítások).sliding(3).foldLeft(0) {
    case (összeg, a :: b :: c :: _) =>
      println(összeg, a, b, c)
      összeg + a
  }

val ügyetlen15 = List(1, 2, 3, 4, 5)
eredmény(ügyetlen15)

val spare16 = List(5, 5, 3)
eredmény(spare16)

val strike24 = List(10, 3, 4)
eredmény(strike24)

val tökéletesJáték = List.fill(12)(10)
eredmény(tökéletesJáték)

val csak299 = List.fill(11)(10) ::: List(9)
eredmény(csak299)

val csak288 = List.fill(10)(10) ::: List(9)
eredmény(csak288)

val csak2 = List.fill(9)(10) ::: List(5, 5)
eredmény(csak2)
