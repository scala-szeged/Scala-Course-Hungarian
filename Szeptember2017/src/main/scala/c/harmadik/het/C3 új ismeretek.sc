

// Teljes fájl beolvasása és karakterenkénti iterálása
// Source.fromFile("Szeptember2017/src/main/scala/c/harmadik/het/csomagolópapír.txt")

// String -é összefűzése
// fájl.mkString

// sorokra bontása
// val iterator = str.lines

// iterátor listáváváltoztatása, így többször is bejárható, println és debugger
// segítségével megjeleníthető
// val dobozok = iterator.toList

// szélesség, magasság és hosszúság
"4x3x5".split('x').map(str => str.toInt)

// Lista elemenkénti átalakítása. Az eredmény is lista
// list.map(doboz => szmh(doboz))

//
Array(4, 3, 5) match {
  case Array(sz, m, h) =>
    println("szélesség = " + sz)
    println("magasság = " + m)
    println("hosszúság = " + h)
}