import scala.annotation.tailrec

object NinetyNineScalaProblems {

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

  def compress[T](ls: List[T]): List[T] = ls.foldRight(List[T]()) {
    (h: T, tail: List[T]) =>
      tail.headOption.fold(List(h)) {
        firstFromTail =>
          if (h == firstFromTail) tail else h :: tail
      }
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
    case (1 , elem) => elem
    case (n , elem) => (n, elem)
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

  def removeAt[T](i: Int, ls: List[T]): (List[T], T) = (i, ls) match {
    case (i, ls) if ls.size <= i => throw new NoSuchElementException
    case (i, ls) =>
      val (first, second) = ls.splitAt(i)
      (first ::: second.tail, second.head)
  }

}
