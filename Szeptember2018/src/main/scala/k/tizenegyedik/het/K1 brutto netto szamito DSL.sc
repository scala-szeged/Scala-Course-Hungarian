import scala.language.postfixOps
import java.text.NumberFormat
import java.util.Locale


val bruttó_bér = 400000.Ft

val szociális_hozzájárulási_adó = 19.5 százalék

bruttó_bér * szociális_hozzájárulási_adó

val szakképzési_hozzájárulás = 1.5 százalék

bruttó_bér * szakképzési_hozzájárulás

val munkaerő_piaci_járulék = 1.5 százalék

bruttó_bér * munkaerő_piaci_járulék

val egészségbiztosítási_járulék = 7 százalék

bruttó_bér * egészségbiztosítási_járulék

val nyugdíj_járulék = 10 százalék

bruttó_bér * nyugdíj_járulék

val számított_SZJA = 15 százalék

bruttó_bér * számított_SZJA


val havi_összes_levonás_a_bruttó_bérből =
  munkaerő_piaci_járulék +
    egészségbiztosítási_járulék +
    nyugdíj_járulék +
    számított_SZJA

bruttó_bér * havi_összes_levonás_a_bruttó_bérből

val havi_összes_munkaadói_járulék =
  szociális_hozzájárulási_adó +
    szakképzési_hozzájárulás

bruttó_bér * havi_összes_munkaadói_járulék

val összesen_havonta_az_államnak_fizetendő =
  havi_összes_levonás_a_bruttó_bérből +
    havi_összes_munkaadói_járulék

bruttó_bér * összesen_havonta_az_államnak_fizetendő


val munkaadó_összes_havi_költsége =
  bruttó_bér +
    havi_összes_munkaadói_járulék

munkaadó_összes_havi_költsége

val nettó_havi_munkabér = bruttó_bér - havi_összes_levonás_a_bruttó_bérből


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

  def *(sz: Százalék) = fizetésDoubleból(n * sz.n / 100)
}

case class Forint(override val n: Double) extends Fizetés(n) {
  override def fizetésDoubleból(n: Double): Fizetés = Forint(n)

  override def toString: String = NumberFormat.getCurrencyInstance(new Locale("hu", "HU")).format(n)
}

case class Euro(override val n: Double) extends Fizetés(n) {
  override def fizetésDoubleból(n: Double): Fizetés = Euro(n)

  override def toString: String = NumberFormat.getCurrencyInstance(new Locale("de", "DE")).format(n)
}


// Lásd: http://berkalkulator.hu/berkalkulator-2018/100000-brutto-fizetes
