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
}
