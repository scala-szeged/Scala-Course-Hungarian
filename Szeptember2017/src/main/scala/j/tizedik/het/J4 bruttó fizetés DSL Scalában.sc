import scala.language.postfixOps
import java.text.NumberFormat
import java.util.Locale

implicit class Százalék(val n: Double) {
  override def toString: String = s"$n %"

  def százalék = this

  def sz = százalék

  def +(sz: Százalék) = Százalék(n + sz.n)
}

implicit class Fizetés(val n: Double) {
  def Ft = Forint(n)

  def Eur = Euro(n)

  def fizetésDoubleból(n: Double): Fizetés = {
    println
    println("ismeretlen típusú pénzösszeget hoztunk létre")
    println(Thread.currentThread().getStackTrace.take(6).mkString("\n  at "))
    new Fizetés(n)
  }

  def -(sz: Százalék) = fizetésDoubleból(n * (1 - sz.n / 100))

  def +(sz: Százalék) = fizetésDoubleból(n * (1 + sz.n / 100))
}

case class Forint(override val n: Double) extends Fizetés(n) {
  override def fizetésDoubleból(n: Double): Fizetés = Forint(n)

  override def toString: String = NumberFormat.getCurrencyInstance(new Locale("hu", "HU")).format(n)
}

case class Euro(override val n: Double) extends Fizetés(n) {
  override def fizetésDoubleból(n: Double): Fizetés = Euro(n)

  override def toString: String = NumberFormat.getCurrencyInstance(new Locale("de", "DE")).format(n)
}

object nettóra_átszámítva {
  def bruttó(bruttó_bér: Fizetés): Fizetés = {

    val munkaerő_piaci_járulék = 1.5 százalék
    val egészségbiztosítási_járulék = 7 sz
    val nyugdíj_járulék = 10 százalék
    val számított_SZJA = 15 százalék

    val havi_összes_levonás_a_bruttó_bérből =
      munkaerő_piaci_járulék +
        egészségbiztosítási_járulék +
        nyugdíj_járulék +
        számított_SZJA

    val szociális_hozzájárulási_adó = 22 százalék
    val szakképzési_hozzájárulás = 1.5 százalék
    val havi_összes_munkaadói_járulék =
      szociális_hozzájárulási_adó +
        szakképzési_hozzájárulás

    val összesen_havonta_az_államnak_fizetendő =
      havi_összes_levonás_a_bruttó_bérből +
        havi_összes_munkaadói_járulék

    val munkaadó_összes_havi_költsége =
      bruttó_bér +
        havi_összes_munkaadói_járulék


    bruttó_bér - havi_összes_levonás_a_bruttó_bérből
  }
}

// A Scala fordító ezt is lefordítja, de a Ft metódust csak a legvégén hívja meg a Scala Worksheet
nettóra_átszámítva bruttó 100000 Ft

// Ez viszont az IntelliJ szerint is jó és az Eur metódust hamarabb hívja mint a bruttó metódust
nettóra_átszámítva bruttó 100000.Eur


// Lásd: http://berkalkulator.hu/berkalkulator-2018/100000-brutto-fizetes
