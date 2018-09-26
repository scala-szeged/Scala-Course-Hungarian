
// Ha nem jelezzük, akkor public trait, public class,
// illetve public metódus amit készítettünk,
// nem pedig package private. Nincs is public
// csak private és protected kulcsszó

// Hasonló a Java interface -ekhez, de abstract adatokat
// is leírhat nemcsak abstarct és konkrét (definiált) metódusokat
trait ListKezelő {

  // Abstract metódus, mivel nem definiált.
  // (Nincsen teste kapcsos zárójelek között, illetve
  // egyenlőség jel után.)
  def keresd(ebben: List[Char], ezt: List[Char]): Int

  // Konkrét, definiált metódus
  def részListaE(teljes: List[Char], rész: List[Char]): Boolean = {
    keresd(teljes, rész) > 0
  }
}

// Az első ős (ez most a ListKezelő trait) előtt
// extends, a továbbiak előtt a with kulcsszó áll.
// Több trait illetve egyetlen class lehet az őse
class RabinKarpStringSearch extends ListKezelő {

  // Lásd: https://en.wikipedia.org/wiki/String-searching_algorithm
  override def keresd(ebben: List[Char], ezt: List[Char]): Int = ???
}
