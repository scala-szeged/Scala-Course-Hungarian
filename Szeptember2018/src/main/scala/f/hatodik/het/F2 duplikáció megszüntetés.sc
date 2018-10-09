
// A megoldás elemei

val (maradékSzó1, gyűjtött1) = ("laxx".toList, List())
(maradékSzó1, gyűjtött1) match {
  case (ez :: többiBetű, List()) => "talált: case (ez :: többiBetű, List())"
  case _ => "nem jó: case (ez :: többiBetű, List())"
}

val (maradékSzó2, gyűjtött2) = ("x".toList, "xal".toList)
(maradékSzó2, gyűjtött2) match {
  case (ez :: többiBetű, előző :: mégEzelőttiek) if ez == előző =>
    "talált: case (ez :: többiBetű, előző :: mégEzelőttiek) if ez == előző"

  case _ =>
    "nem jó: case (ez :: többiBetű, előző :: mégEzelőttiek) if ez == előző"
}

"arrébb".toList.toString
"arrébb".toList.mkString
"arrébb".reverse


// A feladat
def megszüntet(szó: String): String = {

  def loop(maradékSzó: List[Char], gyűjtött: List[Char]): List[Char] = (maradékSzó, gyűjtött) match {

  }

  loop(szó.toList, ???)
}

megszüntet("bob") // eredmény: bob
megszüntet("boob") // eredmény: bob
megszüntet("laxx") // eredmény: lax
megszüntet("boooooobapalaxxxxios") // eredmény: bobapalaxios