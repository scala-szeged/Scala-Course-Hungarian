import java.time.LocalDate

import scala.language.postfixOps

object ezelőtt

object ezután

object ago

implicit class napLesz(n: Int) {
  def nappal(e: ezelőtt.type) = ma.minusDays(n)

  def nappal(u: ezután.type) = ma.plusDays(n)

  def days(a: ago.type) = nappal(ezelőtt)

  def days(f: from.type) = from(n)

  private def ma = LocalDate.now
}

case class from(n: Int) extends napLesz(n) {
  val now = nappal(ezután)
}


val ötNappalEzelőtt = 5 nappal ezelőtt
val ötNappalEzután = 5 nappal ezután
val fiveDaysAgo = 3 days ago
val fiveDaysFromNow = 3 days from now
