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
