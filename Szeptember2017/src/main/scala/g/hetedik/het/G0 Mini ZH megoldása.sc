
def szorzat(számok: List[Char], n: Long = 1): Long = számok match {
  case Nil => n
  case h :: t => szorzat(t, n * (h - 48))
}

szorzat("7316717653133".toList)

def keresd(esz: List[Char], n: Long = 0): Long = esz match {
  case _ :: Nil =>
    n

  case a :: b :: t =>
    val újn = (a - 48) * (b - 48)
    if (újn > n) keresd(b :: t, újn)
    else keresd(b :: t, n)
}

keresd("7316717653133".toList)
