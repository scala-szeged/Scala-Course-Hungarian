
def szorzat(számok: List[Char], n: Long = 1): Long = számok match {
  case Nil => n
  case h :: t => szorzat(t, n * (h - '0'))
}

szorzat("7316717653133".toList)

def keresd(esz: List[Char], n: Long = 0): Long = esz match {
  case _ :: Nil =>
    n

  case Nil =>
    n

  case a :: b :: t =>
    val újn = (a - '0') * (b - '0')
    if (újn > n) keresd(b :: t, újn)
    else keresd(b :: t, n)
}

keresd("7316717653133".toList)
keresd(Nil)
