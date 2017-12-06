package m.tizenharmadik.het

object M1_hibalehetőséggel extends App {


  class Készülék(blueToothId: Long)

  case class Hőmérő(blueToothId: Long) extends Készülék(blueToothId)

  case class VérnyomásMérő(blueToothId: Long) extends Készülék(blueToothId)

  case class VércukorMérő(blueToothId: Long) extends Készülék(blueToothId)

  case class MagzatSzívhangMérő(blueToothId: Long) extends Készülék(blueToothId)

  case class Ekg(blueToothId: Long) extends Készülék(blueToothId)


  class Személy(név: String)

  case class Beteg(név: String) extends Személy(név)

  case class Orvos(név: String) extends Személy(név)


  sealed trait Akció

  case class ÚjKészülékBekerülés(készülék: Készülék) extends Akció

  case class Kihelyezés(készülék: Készülék, beteg: Beteg, orvos: Orvos) extends Akció

  case class Visszaadás(készülék: Készülék, beteg: Beteg, orvos: Orvos) extends Akció

  case class Karbantartás(készülék: Készülék, beteg: Beteg) extends Akció


  case class Készülékek(készlet: Set[Készülék] = Set(), kiadva: Set[Készülék] = Set(), javításon: Set[Készülék] = Set()) {

    def csináld(akciók: Akció*): Készülékek = csináld(akciók.toList)

    def csináld(akciók: List[Akció]): Készülékek = akciók match {
      case Nil => this
      case ÚjKészülékBekerülés(k) :: ak => Készülékek(készlet + k, /*ITT A HIBA*/ javításon, kiadva).csináld(ak)
    }
  }

  assert(
    Készülékek().csináld(ÚjKészülékBekerülés(Hőmérő(1)))
      == Készülékek(készlet = Set(Hőmérő(1)))
  )

  assert(
    Készülékek(
      készlet = Set(Hőmérő(1)), kiadva = Set(VérnyomásMérő(2)), javításon = Set(VércukorMérő(3))
    ).csináld(ÚjKészülékBekerülés(Ekg(4)))

      == Készülékek(készlet = Set(Hőmérő(1), Ekg(4)))
  )
}
