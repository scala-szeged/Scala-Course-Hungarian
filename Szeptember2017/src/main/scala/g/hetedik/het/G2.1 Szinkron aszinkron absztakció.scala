package g.hetedik.het


import scala.concurrent._
import scala.concurrent.duration.Duration
import ExecutionContext.Implicits.global


// https://github.com/fommil/fpmortals/blob/master/src/main/scala/chapter1.scala
// https://leanpub.com/fpmortals/read
// Copyright: 2017 Sam Halliday
// License: https://creativecommons.org/publicdomain/zero/1.0/

//noinspection LanguageFeature
object G2_1_Szinkron_Aszinkron_Absztakció

/*{


 import Execution.Ops

 def echo[P[_]](implicit t: Terminal[P], e: Execution[P]): P[String] =
   for {
     in <- t.read
     _ <- t.write(in)
   } yield in

 type Now[X] = X

 implicit val now: Terminal[Now] = TerminalSync
 implicit val future: Terminal[Future] = new TerminalAsync

 def main(args: Array[String]): Unit = {
   // interpret for Now
   echo[Now]: Now[String]

   // interpret for Future
   val running: Future[String] = echo[Future]
   Await.result(running, Duration.Inf)
 }

 //noinspection LanguageFeature
 trait Terminal[P[_]] {
   def read: P[String]

   def write(t: String): P[Unit]
 }

 object TerminalSync extends Terminal[Now] {
   def read: String = io.StdIn.readLine

   def write(t: String): Unit = println("Now", t)
 }

 class TerminalAsync(implicit EC: ExecutionContext) extends Terminal[Future] {

   def read: Future[String] = Future {
     val fájl = scala.io.Source.fromURL(
       "https://raw.githubusercontent.com/fommil/fpmortals/master/src/main/scala/chapter1.scala"
     )
     fájl.mkString
   }

   def write(t: String): Future[Unit] = Future {
     println("Future", t)
   }
 }

 trait Execution[P[_]] {
   def doAndThen[A, B](c: P[A])(f: A => P[B]): P[B]

   def create[B](b: B): P[B]
 }

 object Execution {

   implicit class Ops[A, P[_]](c: P[A]) {
     def flatMap[B](f: A => P[B])(implicit e: Execution[P]): P[B] =
       e.doAndThen(c)(f)

     def map[B](f: A => B)(implicit e: Execution[P]): P[B] =
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

}
*/