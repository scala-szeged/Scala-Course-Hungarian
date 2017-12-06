package m.tizenharmadik.het

object M2_típusos_programozás extends App {


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
  case class Javítás(készülék: Készülék) extends Akció


  case class Készlet(set: Set[Készülék] = Set()) extends AnyVal
  case class Kiadva(set: Set[Készülék] = Set()) extends AnyVal
  case class Javításon(set: Set[Készülék] = Set()) extends AnyVal


  case class Nyilvántartás(készlet: Készlet = Készlet(),
                           kiadva: Kiadva = Kiadva(),
                           javításon: Javításon = Javításon()) {

    def csináld(akciók: Akció*): Nyilvántartás = csináld(akciók.toList)

    def csináld(akciók: List[Akció]): Nyilvántartás = akciók match {
      case Nil =>
        this

      case ÚjKészülékBekerülés(k) :: ak =>
        Nyilvántartás(Készlet(készlet.set + k), kiadva, javításon).csináld(ak)

      case Kihelyezés(k, b, d) :: ak =>
        Nyilvántartás(Készlet(készlet.set - k), Kiadva(kiadva.set + k), javításon)

      case Visszaadás(k, b, d) :: ak =>
        Nyilvántartás(Készlet(készlet.set + k), Kiadva(kiadva.set - k), javításon)

      case Javítás(k) :: ak =>
        Nyilvántartás(Készlet(készlet.set - k), kiadva, Javításon(javításon.set + k))
    }
  }

  assert(
    Nyilvántartás().csináld(ÚjKészülékBekerülés(Hőmérő(1)))
      == Nyilvántartás(Készlet(Set(Hőmérő(1))))
  )

  assert(
    Nyilvántartás(
      Készlet(Set(Hőmérő(1))), Kiadva(Set(VérnyomásMérő(2))), Javításon(Set(VércukorMérő(3)))
    ).csináld(ÚjKészülékBekerülés(Ekg(4)))

      == Nyilvántartás(
      Készlet(Set(Hőmérő(1), Ekg(4))),
      Kiadva(Set(VérnyomásMérő(2))),
      Javításon(Set(VércukorMérő(3)))
    )
  )

  val nyilvántartás = Nyilvántartás(Készlet(Set(MagzatSzívhangMérő(1), MagzatSzívhangMérő(2))))

  assert(
    nyilvántartás
      .csináld(Kihelyezés(MagzatSzívhangMérő(1), Beteg("Nagy Erika"), Orvos("Sári Tamás")))
      .csináld(Kihelyezés(MagzatSzívhangMérő(2), Beteg("Kiss Anna"), Orvos("Sári Tamás")))

      == Nyilvántartás(kiadva = Kiadva(Set(MagzatSzívhangMérő(1), MagzatSzívhangMérő(2))))
  )

  assert(
    nyilvántartás
      .csináld(Kihelyezés(MagzatSzívhangMérő(1), Beteg("Nagy Erika"), Orvos("Sári Tamás")))
      .csináld(Visszaadás(MagzatSzívhangMérő(1), Beteg("Nagy Erika"), Orvos("Sári Tamás")))

      == nyilvántartás
  )

  assert(
    nyilvántartás.csináld(Javítás(MagzatSzívhangMérő(1)))

      == Nyilvántartás(
      készlet = Készlet(Set(MagzatSzívhangMérő(2))),
      javításon = Javításon(Set(MagzatSzívhangMérő(1)))
    )
  )
}
