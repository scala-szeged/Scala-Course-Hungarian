import java.text.NumberFormat
import java.util.Locale

object ÁFA {
  def unary_+ = PluszÁFA
}

object PluszÁFA

implicit class Pénzösszeg(val n: Double) {
  def Ft = Forint(n)

  def Ft(pluszÁFA: PluszÁFA.type) = Forint(n * 1.27)

  def +(áfa: ÁFA.type) = Forint(n * 1.27)
}

//import java.util.Currency
case class Forint(override val n: Double) extends Pénzösszeg(n) {
  override def toString: String = NumberFormat.getCurrencyInstance(new Locale("hu", "HU")).format(n)
}

100 Ft

(100 Ft)
100 Ft;
100 Ft +ÁFA
(100 Ft) + ÁFA

object fizetés {
  def eltartottakSzáma = ???

  def bruttó(pénzösszeg: Pénzösszeg): Pénzösszeg = {

    // https://www.hrportal.hu/berkalkulator_2017.html?edt_brutto=100000&edt_elt=0&edt_gyerekek=0&edt_egyedulnevel=0&edt_frisshazas=0&edt_kedvezmeny=0&edt_munkaido=40
    val évesBruttó = pénzösszeg.n * 12

    pénzösszeg.n
  }
}

// A Scala fordító lefordítja
fizetés bruttó 100000 Ft