import scala.language.postfixOps
import java.text.NumberFormat
import java.util.Locale

implicit class Százalék(val n: Double) {
  override def toString: String = s"$n %"

  def %% = this

  def +(sz: Százalék) = Százalék(n + sz.n)
}

implicit class Pénzösszeg(val n: Double) {
  def Ft = Forint(n)

  def -(sz: Százalék) = n * (1 - sz.n / 100)
  def +(sz: Százalék) = n * (1 + sz.n / 100)
}

//import java.util.Currency
case class Forint(override val n: Double) extends Pénzösszeg(n) {
  override def toString: String = NumberFormat.getCurrencyInstance(new Locale("hu", "HU")).format(n)
}

object fizetés {
  def bruttó(pénzösszeg: Pénzösszeg): Pénzösszeg = {

    val munkaerőPiacijárulék = 1.5 %%
    val egészségbiztosításijárulék = 7.0 %%
    val nyugdíjjárulék = 10.0 %%
    val számítottSzja = 15.0 %%

    val haviösszeslevonásabruttóbérből =
      munkaerőPiacijárulék +
        egészségbiztosításijárulék +
        nyugdíjjárulék +
        számítottSzja

    val szociálishozzájárulásiadó = 22.0 %%
    val szakképzésihozzájárulás = 1.5 %%
    val haviösszesmunkaadóijárulék =
      szociálishozzájárulásiadó +
        szakképzésihozzájárulás

    val összesenhavontaazállamnakfizetendő =
      haviösszeslevonásabruttóbérből +
    haviösszesmunkaadóijárulék

    val munkaadóösszeshaviköltsége =
      pénzösszeg +
    haviösszesmunkaadóijárulék


    println(haviösszeslevonásabruttóbérből)

    pénzösszeg - haviösszeslevonásabruttóbérből
  }
}

// A Scala fordító lefordítja
fizetés bruttó 100000 Ft


// Lásd: http://berkalkulator.hu/berkalkulator-2018/100000-brutto-fizetes
