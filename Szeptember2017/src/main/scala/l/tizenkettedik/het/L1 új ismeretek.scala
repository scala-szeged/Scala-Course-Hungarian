package l.tizenkettedik.het

//noinspection ScalaFileName
object L1_új_ismeretek extends App {

  // shuffle
  val összekevertElelemek = scala.util.Random.shuffle(Vector(1,2,3,4))
  println("összekevertElelemek", összekevertElelemek)


  // Vector
  // Olyan collection, amelynek bármely elemét nagyjából konstans idő alatt lehet elérni,
  // és az elejére, illetve végére is gyors a hozzáfűzés


  // ++
  // Összefűz két vektort
  val összefűzött = Vector(3, 4) ++ Vector(6, 7)
  println("összefűzött", összefűzött)

  // +:
  // új elem hozzáadása a vektor elejére
  val elejére = 9 +: Vector(1, 2)
  println("elejére", elejére)


  // :+
  // új elem hozzáadása a vektor végére
  val végére = Vector(1, 2) :+ 9
  println("végére", végére)


  // Két Vector
  // _
  //    Itt azt jelenti az _ hogy nem csinálink semmit az egytől háromig terjedő számokkal
  // csak három iterációt csinálunk
  for (_ <- 1 to 3) print("ugyanaz")


  // fold
  // Ez a collection metódus egy kezdőértéket és egy másik metódust vár.
  // Az a másik metódus asszociatív, ezért fold támogatja a párhuzamos futást.
  // Az a másik metódus 2 bemenő értéke és a kimenő érték ugyanolyan típusú


  // max
  // Ez a collection metódus a legnagyobb értéket adja vissza az elemek küzül


  println
  Thread.sleep(3000)

  ??? // Ezt az exception -t dobja:
  // Exception in thread "main" scala.NotImplementedError: an implementation is missing
}