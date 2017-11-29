package l.tizenkettedik.het

//noinspection ScalaFileName
object L2_párhuzamos_map_fold_stb {


  def main(args: Array[String]): Unit = {

    val n = 5

    val ev: Vector[Int] = scala.util.Random.shuffle(Vector.tabulate(2000000)(i => i))
    val v: Vector[Int] = ev.map(i => i + 10)

    val mapmaxa = for (_ <- 1 to n) yield time(
      ev.map(i => i + 10).max,
      "ev.map(i => i+n).max"
    )
    val mapmaxb = for (_ <- 1 to n) yield time(
      ev.par.map(i => i + 10).max,
      "ev.par.map(i => i+n).max"
    )

    def maxIffel(a: Int, b: Int): Int = if (a > b) a else b

    val folda = for (_ <- 1 to n) yield time(
      v.fold(0)(maxIffel),
      "v.fold(0) { maxIffel }"
    )
    val foldb = for (_ <- 1 to n) yield time(
      v.par.fold(0)(maxIffel),
      "v.par.fold(0) { maxIffel }"
    )

    val aggregateA = for (_ <- 1 to n) yield time(
      v.aggregate(0)(maxIffel, { (z, a) => ??? }),
      "v.aggregate(0)(maxIffel, { (z, a) => ??? })"
    )
    val aggregateB = for (_ <- 1 to n) yield time(
      v.par.aggregate(0)(maxIffel, maxIffel),
      "v.par.aggregate(0)(maxIffel, maxIffel)"
    )

    val maxa = for (_ <- 1 to n) yield time(v.max, "v.max")
    val maxb = for (_ <- 1 to n) yield time(v.par.max, "v.par.max")

    val csupaEgyformaEredmény =
      mapmaxa ++ mapmaxb ++ folda ++ foldb ++ aggregateA ++ aggregateB ++ maxa ++ maxb
    assert(csupaEgyformaEredmény.distinct.size == 1)
    assert(csupaEgyformaEredmény.head == 2000000 - 1 + 10)
  }

  def time[R](block: => R, msg: String): R = {
    val t0 = System.currentTimeMillis
    val result = block
    val t1 = System.currentTimeMillis
    println(msg + ": " + (t1 - t0) + "ms")
    result
  }
}