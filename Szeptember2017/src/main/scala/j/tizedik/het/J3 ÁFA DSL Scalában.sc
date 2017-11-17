import scala.language.postfixOps
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

case class Forint(override val n: Double) extends Pénzösszeg(n) {
  override def toString: String = NumberFormat.getCurrencyInstance(new Locale("hu", "HU")).format(n)
}

1000000 Ft

val első_száz = 100 Ft
val első_száz_plusz_ÁFA = első_száz + ÁFA

val második_száz_plusz_ÁFA = 100 Ft +ÁFA
val harmadik_száz_plusz_ÁFA = (100 Ft) + ÁFA


