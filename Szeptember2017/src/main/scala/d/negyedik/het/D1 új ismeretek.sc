
// --- For comprehension, 4. hét

// mindhárom ugyanaz
// Lásd bővebben: https://docs.scala-lang.org/tutorials/FAQ/yield.html
val xs = for (x <- List(9, 8, 7)) yield (x / 2)
val ys = for (y <- List(9, 8, 7)) yield y / 2
val zs = List(9, 8, 7).map(z => z / 2)

// for - nak lehet Unit típusú törzse is, map -nek nem

for (y <- List(9, 8, 7)) {
  println(y / 2)
}

// Egy for több iterációt is végrehajthat

val sakkTáblaSet = for {
  sor <- Set('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h')
  oszlop <- List(1, 2, 3, 4, 5, 6, 7, 8)
} yield (sor, oszlop)

sakkTáblaSet.size

// hány féleképpen lehet 2 kockával adott számot dobni?

def variációk(n: Int): Int = {
  val vk = for {
    e <- 1 to 6
    m <- 1 to 6
    if e + m == n
  } yield (e, m)

  vk.size
}

val féleképpen = for (n <- 2 to 12) yield (n, variációk(n))




// --- Lista kezelés, 4. hét


// ::: listák összefűzése

List(3, 4) ::: List(5, 6, 7)

List(List(1,2),List(3, 4)) ::: List(List(5, 6, 7))


// :: elem hozzáfűzése a lista elejére

List(3, 4) :: List(List(5, 6, 7))

List(1,2) :: List(3, 4) :: List(List(5, 6, 7))

