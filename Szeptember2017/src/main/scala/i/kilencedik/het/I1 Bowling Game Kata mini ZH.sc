def végNullák(gurítások: List[Int]) =
  (gurítások.init.last, gurítások.last) match {
    case (10, 10) => gurítások
    case (10, z) if z < 10 => gurítások ::: List(0)
    case (y, z) => gurítások ::: List(0, 0)
  }

def eredmény(gurítások: List[Int]) =
  végNullák(gurítások).sliding(3).foldLeft(0) {
    case /*strike*/ (összeg, 10 :: b :: c :: Nil) =>
      println(összeg, 'X', b, c)
      összeg + 10 + b + c

    case /*spare*/ (összeg, a :: b :: c :: Nil) if a + b == 10 =>
      println(összeg, a, '/', c)
      összeg + a + c

    case (összeg, a :: b :: c :: Nil) =>
      println(összeg, a, b, c)
      összeg + a
  }

val egySpareNemKetto23 = List(5, 5, 5, 3)
eredmény(egySpareNemKetto23)

val kettoSpareNemNegy36 = List(5, 5, 5, 5, 3)
eredmény(kettoSpareNemNegy36)

val ügyetlen15 = List(1, 2, 3, 4, 5)
eredmény(ügyetlen15)

val spare16 = List(5, 5, 3)
eredmény(spare16)

val strike24 = List(10, 3, 4)
eredmény(strike24)

val tökéletesJáték300 = List.fill(12)(10)
eredmény(tökéletesJáték300)

val csak299 = List.fill(11)(10) ::: List(9)
eredmény(csak299)

val csak288 = List.fill(10)(10) ::: List(9)
eredmény(csak288)

val csak265 = List.fill(9)(10) ::: List(5, 5)
eredmény(csak265)
