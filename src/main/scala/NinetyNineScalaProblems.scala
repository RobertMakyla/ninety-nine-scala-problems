import scala.annotation.tailrec

object NinetyNineScalaProblems {

  //implement a higher-order function multiplyBy(Double): Double => Double
  def multiplyBy(factor: Double): Double => Double = (x: Double) => factor * x
  def tenTimes: Double => Double = multiplyBy(10)
  def twenty: Double = tenTimes(2)

  // currying
  def times(x:Int, y:Int) = x * y
  def timesOneAtTheTime(f:Int) = (x:Int) => x * f //curried function

  @tailrec
  def last[T](ls: List[T]): T = ls match {
    case Nil => throw new NoSuchElementException
    case h :: Nil => h
    case _ :: tail => last(tail)
  }

  @tailrec
  def penultimate[T](ls: List[T]): T = ls match {
    case _ if ls.size <= 1 => throw new NoSuchElementException
    case beforeLast :: _ :: Nil => beforeLast
    case _ :: tail => penultimate(tail)
  }

  @tailrec
  def nth[T](n: Int, ls: List[T]): T = ls match {
    case Nil => throw new NoSuchElementException
    case h :: _ if n == 0 => h
    case _ :: tail => nth(n - 1, tail)
  }

  @tailrec
  def length(ls: List[_], total: Int = 0): Int = ls match {
    case Nil => total
    case _ :: tail => length(tail, total + 1)
  }

  @tailrec
  def reverse[T](ls: List[T], total: List[T] = Nil): List[T] = ls match {
    case Nil => total
    case h :: tail => reverse(tail, h :: total)
  }

  @tailrec
  def isPalindrome[T](ls: List[T]): Boolean = ls match {
    case _ if ls.size < 2 => true
    case h :: last :: Nil => h == last
    case _ => ls.head == ls.last && isPalindrome(ls.init.tail)
  }

  def flatten(ls: List[Any]): List[Any] = ls.flatMap {
    case l: List[_] => flatten(l)
    case t => List(t)
  }

  def compress[T](ls: List[T]): List[T] = ls match {
    case Nil => Nil
    case h :: Nil => ls
    case h :: a :: tail if h == a => compress(a :: tail)
    case h :: a :: tail => h :: compress(a :: tail)
  }

  @tailrec
  def compressTailRec[T](ls: List[T], total: List[T] = Nil): List[T] = ls match {
    case h :: tail => compressTailRec(tail.dropWhile(_ == h), h :: total)
    case empty => total.reverse
  }

  /**
   * span() eg.: List(1, 2, 3, -4, 5) span (_ > 0)
   * returns tuple:  (List(1, 2, 3), List(-4, 5))
   */
  def pack[T](ls: List[T]): List[List[T]] = ls match {
    case Nil => List.empty[List[T]]
    case h :: tail =>
      val (firstPack, rest) = tail.span(_ == h)
      List(h :: firstPack) ++ pack(rest)
  }

  @tailrec
  def packTailRec[T](ls: List[T], total: List[List[T]] = Nil): List[List[T]] = ls match {
    case Nil => total
    case h :: tail =>
      val (firstPack, rest) = tail.span(_ == h)
      packTailRec(rest, total ++ List(h :: firstPack))
  }

  def encode[T](ls: List[T]): List[(Int, T)] = pack(ls).map {
    pack =>
      pack.headOption.fold(throw new RuntimeException("list should not be empty")) { head =>
        (pack.size, head)
      }
  }

  def encodeModified[T](ls: List[T]): List[Any] = encode(ls).map {
    case (1, elem) => elem
    case (n, elem) => (n, elem)
  }

  def decode[T](ls: List[(Int, T)]): List[T] = ls.flatMap {
    case (n, elem) => List.fill(n)(elem)
  }

  def encodeDirect[T](ls: List[T]): List[(Int, T)] = if (ls.isEmpty) Nil
  else {
    val (firstPack, rest) = ls.span(_ == ls.head)
    (firstPack.size, firstPack.head) :: encodeDirect(rest)
  }

  def duplicate[T] = duplicateN[T](2) _

  def duplicateN[T](n: Int)(ls: List[T]): List[T] = ls.flatMap { e => List.fill(n)(e) }

  def drop[T](n: Int, ls: List[T]): List[T] = ls.zipWithIndex.filterNot {
    case (_, index) => (index + 1) % n == 0
  }.map(_._1)

  def slice[T](index: Int, ls: List[T]) = ls.splitAt(index)

  def slice[T](start: Int, end: Int, ls: List[T]): List[T] = {
    val (_, afterStart) = ls.splitAt(start)
    val (beforeEnd, _) = afterStart.splitAt(end - start)
    beforeEnd
  }

  def rotate[T](i: Int, ls: List[T]): List[T] = ls match {
    case _ if i == 0 => ls
    case h :: tail if i > 0 => rotate(i - 1, tail ++ List(h))
    case _ if i < 0 => rotate(i + 1, ls.last :: ls.init)
  }

  def removeAt[T](i: Int, ls: List[T]): (List[T], T) =
    if (ls.size <= i) throw new NoSuchElementException
    else {
      val (first, second) = ls.splitAt(i)
      (first ::: second.tail, second.head)
    }

  def insertAt[T](e: T, i: Int, ls: List[T]): List[T] =
    if (ls.size < i) throw new NoSuchElementException
    else {
      val (first, second) = ls.splitAt(i)
      first ++ (e :: second)
    }

  @tailrec
  def range(start: Int, end: Int, total: List[Int] = Nil): List[Int] = if (end > start) {
    range(start + 1, end, start :: total)
  } else if (end < start) {
    range(start - 1, end, start :: total)
  }
  else total.reverse ++ List(start)

  private def random(i: Int) = scala.util.Random.nextInt(i)

  @tailrec
  def randomSelect[T](n: Int, ls: List[T], total: List[T] = Nil): List[T] = if (ls.size < n) throw new RuntimeException("list is too small")
  else if (n < 0) throw new RuntimeException(s"$n is negative")
  else if (n == 0) total
  else {
    val pos = random(ls.size)
    val (first, second) = ls.splitAt(pos)
    randomSelect(n - 1, first ++ second.tail, second.head :: total)
  }

  def lotto(n: Int, max: Int, used: List[Int] = Nil): List[Int] = randomSelect(n, 1.to(max).toList)

  @tailrec
  def randomPermute[T](ls: List[T], total: List[T] = Nil): List[T] = ls match {
    case Nil => total
    case _ =>
      val pos = random(ls.size)
      val (first, second) = ls.splitAt(pos)
      randomPermute(first ++ second.tail, second.head :: total)
  }

  def combinations[T](n: Int, ls: List[T]): List[List[T]] =
    if (n <= 0 || n > ls.size) Nil
    else ls match {
      case Nil => Nil
      case _ if n == 1 => ls.map(List(_))
      case h :: tail =>
        val pairsWithHead = combinations(n - 1, tail).map { t =>
          List(h) ++ t
        }
        pairsWithHead ++ combinations(n, tail)
    }

  def lsort[T](ls: List[List[T]]): List[List[T]] = ls.sortWith(_.length < _.length)

}
