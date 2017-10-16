
// --- Ez nem része a számonkérésnek, csak érdekesség

val számok = Stream.from(2).take(10).toList

val fib1: Stream[Int] = 0 #:: fib1.scanLeft(1)(_ + _)

def fib2: Stream[Int] = 0 #:: 1 #:: (fib2 zip fib2.tail).map {
  case (előző, előzőElőtti) => előző + előzőElőtti
}

def fakt: Stream[Int] = 1 #:: (fakt zip Stream.from(2)).map {
  case (előző, n) => előző * n
}

fib1.take(10).toList
fib2.take(10).toList
fakt.take(10).toList
