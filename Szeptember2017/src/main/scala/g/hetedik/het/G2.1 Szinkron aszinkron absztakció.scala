package g.hetedik.het


import scala.concurrent._
import scala.concurrent.duration.Duration


// https://github.com/fommil/fpmortals/blob/master/src/main/scala/chapter1.scala
// https://leanpub.com/fpmortals/read
// Copyright: 2017 Sam Halliday
// License: https://creativecommons.org/publicdomain/zero/1.0/

object G2_1_Szinkron_Aszinkron_Absztakció {

  import Execution.Ops
  import ExecutionContext.Implicits.global

  def echo[C[_]](implicit t: Terminal[C], e: Execution[C]): C[String] =
    for {
      in <- t.read
      _ <- t.write(in)
    } yield in

  implicit val now: Terminal[Now] = TerminalSync
  implicit val future: Terminal[Future] = new TerminalAsync

  def main(args: Array[String]): Unit = {
    // interpret for Now
    echo[Now]: Now[String]

    // interpret for Future
    val running: Future[String] = echo[Future]
    Await.result(running, Duration.Inf)
  }
}

object `package` {
  type Now[X] = X
}

trait Terminal[C[_]] {
  def read: C[String]

  def write(t: String): C[Unit]
}

object TerminalSync extends Terminal[Now] {
  def read: String = io.StdIn.readLine

  def write(t: String): Unit = println("Now", t)
}

class TerminalAsync(implicit EC: ExecutionContext) extends Terminal[Future] {

  def read: Future[String] = Future {
    val fájl = scala.io.Source.fromURL("https://raw.githubusercontent.com/fommil/fpmortals/master/src/main/scala/chapter1.scala")
    fájl.mkString
  }

  def write(t: String): Future[Unit] = Future {
    println("Future", t)
  }
}

trait Execution[C[_]] {
  def doAndThen[A, B](c: C[A])(f: A => C[B]): C[B]

  def create[B](b: B): C[B]
}

object Execution {

  implicit class Ops[A, C[_]](c: C[A]) {
    def flatMap[B](f: A => C[B])(implicit e: Execution[C]): C[B] =
      e.doAndThen(c)(f)

    def map[B](f: A => B)(implicit e: Execution[C]): C[B] =
      e.doAndThen(c)(f andThen e.create)
  }

  implicit val now: Execution[Now] = new Execution[Now] {
    def doAndThen[A, B](c: A)(f: A => B): B = f(c)

    def create[B](b: B): B = b
  }

  implicit def future(implicit EC: ExecutionContext): Execution[Future] =
    new Execution[Future] {
      def doAndThen[A, B](c: Future[A])(f: A => Future[B]): Future[B] =
        c.flatMap(f)

      def create[B](b: B): Future[B] = Future.successful(b)
    }

}
