
// Az alábbi figyelmeztetés kikapcsolása
// "Advanced language feature: postfix operation notation"
// FONTOS: Ctrl + B -vel a definícióra ugrani és
// elolvasni, hogy akkor minek van ez a feature egyáltalán?
import scala.language.postfixOps

// Ft és más pénznem helyes kiírása
import java.text.NumberFormat

// Együttműködik a NumberFormat -tal
import java.util.Locale

val százFt = NumberFormat.getCurrencyInstance(
  new Locale("hu", "HU")
).format(100)




import java.time.LocalDate

val ma = LocalDate.now
val kétNappalEzelőtt = ma.minusDays(2)




object ezelőtt

// Int -et konvertál napLesz típusúra, amely
// rendelkezik az alábbi metódusokkal
implicit class napLesz(n: Int) {
  def nappal(e: ezelőtt.type) = ma.minusDays(n)

  private def ma = LocalDate.now
}

val ötNappalEzelőtt = 5 nappal ezelőtt




// "unary_+" a + definíciója a +ÁFA esetben
// olyan mint amikor +1
// illetve -1 szerepel megelőző szám illetve kifejezés nélkül
object ÁFA {
  def unary_+ = PluszÁFA
}

object PluszÁFA

implicit class Pénzösszeg(val n: Double) {
  def Ft(pluszÁFA: PluszÁFA.type) = Pénzösszeg(n * 1.27)

  override def toString: String = s"Pénzösszeg($n)"
}

100 Ft +ÁFA




// s" ... " lehetővé teszi, hogy a string belsejében
// hivatkozni tudjunk az n konstansra az elé $ jelt írva
implicit class Százalék(val n: Double) {
  override def toString: String = s"$n %"

  // a % metódust nem lehet felüldefiniálni az
  // Int és a Double típusra sem
  def %% = this
}