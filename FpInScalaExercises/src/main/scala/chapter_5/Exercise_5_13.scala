package chapter_5

object Exercise_5_13 {

  def unFold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case None => Stream.empty[A]
    case Some((a, s)) => Stream.cons(a, unFold(s)(f))
  }

  implicit class MyStreamUsingUnfold[A](as: Stream[A]) {

    // todo: take, takeWhile, zipAll
    def myMap[B](f: A => B): Stream[B] = unFold(as) {
      case Stream.Empty => None
      case aHead #:: aTail => Some(f(aHead), aTail)
    }

    def myZipWith[B](bs: Stream[B]): Stream[(A, B)] = unFold((as, bs)) {
      case (Stream.Empty, _) => None
      case (_, Stream.Empty) => None
      case (a #:: aTail, b #:: bTail) => Some(((a, b), (aTail, bTail)))
    }

    def myZipAll[B](bs: Stream[B]): Stream[(Option[A], Option[B])] = unFold((as, bs)) {
      case (Stream.Empty, Stream.Empty) => None
      case (Stream.Empty, b #:: bTail) => Some(((None, Some(b)), (Stream.empty[A], bTail)))
      case (a #:: aTail, Stream.Empty) => Some(((Some(a), None), (aTail, Stream.empty[B])))
      case (a #:: aTail, b #:: bTail) => Some(((Some(a), Some(b)), (aTail, bTail)))
    }

    def myTails: Stream[Stream[A]] = unFold(as) {
      case Stream.Empty => None
      case a #:: t => Some((Stream.cons(a, t), t))
    }

    def myTake(n: Int): Stream[A] = unFold((as, n)) {
      case (Stream.Empty, _) => None
      case (_, 0) => None
      case (aHead #:: aTail, nn) => Some(aHead, (aTail, nn - 1))
    }

    def myTakeWhile(p: A => Boolean): Stream[A] = unFold(as) {
      case Stream.Empty =>
        None
      case aHead #:: aTail =>
        if (p(aHead)) Some((aHead, aTail))
        else None
    }

    def stringStream(n: Int): String = as.take(n).toList.mkString("Stream(", ",", ")") // test only method
  }

  /**
    * See the right solution from Cale Gibbard at http://lambda-the-ultimate.org/node/1277#comment-14313
    * "isSubstringOf x y = any (isPrefixOf x) (tails y)"
    */
  def hasSubsequence[A](whole: Stream[A], part: Stream[A]): Boolean = whole.myTails.exists(_ startsWith part)

  def find[A](whole: Stream[A], part: Stream[A]): Int = whole.myTails.zipWithIndex.find { case (s, _) => s startsWith part }.map { case (_, i) => i } getOrElse (-1)

  def hasSubsequence_0[A](whole: Stream[A], part: Stream[A]): Boolean = whole match {
    case Stream.Empty =>
      false

    case _ #:: wTail =>
      if (whole.myZipWith(part).forall { case (w, p) => w == p })
        true
      else
        hasSubsequence(wTail, part)
  }

  def testMap(whole: Stream[Int]): Unit = {
    println(s"the original: ${whole.stringStream(40)}")
    println(s" after myMap: ${whole.myMap(_ + 10).stringStream(40)}")
  }

  def testZipWith[A, B](as: Stream[A], bs: Stream[B]): Unit = {
    println(s"as: ${as.stringStream(40)}")
    println(s"bs: ${bs.stringStream(40)}")
    println(s" as.zipWith(bs): ${as.myZipWith(bs).stringStream(40)}")
  }

  def testZipAll[A, B](as: Stream[A], bs: Stream[B]): Unit = {
    println(s"as: ${as.stringStream(40)}")
    println(s"bs: ${bs.stringStream(40)}")
    println(s" as.zipAll(bs): ${as.myZipAll(bs).stringStream(40)}")
  }

  def testTails(s: Stream[Int]): Unit = {
    println(s"the stream: ${s.stringStream(40)}")
    println(s"the tails: ${s.myTails.stringStream(40)}")
  }

  def testTake(s: Stream[Int], n: Int): Unit = {
    println(s"the stream s: ${s.stringStream(40)}")
    println(s"s.take($n): ${s.myTake(n).stringStream(40)}")
  }

  def testTakeWhile(s: Stream[Int], p: Int => Boolean): Unit = {
    println(s"the stream s: ${s.stringStream(40)}")
    println(s"s.takeWhile(): ${s.myTakeWhile(p).stringStream(40)}")
  }

  def testHasSubsequence[A](whole: Stream[A], part: Stream[A]): Unit = {
    print("hasSubsequence is ")
    print(hasSubsequence(whole, part))
    print(" for (part,whole): ")
    println(part.stringStream(20), whole.stringStream(40))
  }

  def testFind[A](whole: Stream[A], part: Stream[A]): Unit = {
    print("find is ")
    print(find(whole, part))
    print(" for (part,whole): ")
    println(part.stringStream(20), whole.stringStream(40))
  }

  def main(args: Array[String]): Unit = {
    val whole = Stream(1, 2, 3, 4, 5, 6, 7, 8, 9)
    testMap(whole)
    println
    testZipWith(whole, Stream('a', 'b'))
    println
    testZipAll(whole, Stream('a', 'b'))
    println
    testTails(whole)
    println
    testTake(whole, 5)
    println
    testTakeWhile(whole, _ < 8)
    println
    testHasSubsequence(whole, Stream(4, 5))
    testHasSubsequence(whole, Stream(1, 2, 9, 4, 5))
    println
    testFind(whole, Stream(4, 5))
    testFind(whole, Stream(1, 2, 9, 4, 5))
  }
}
