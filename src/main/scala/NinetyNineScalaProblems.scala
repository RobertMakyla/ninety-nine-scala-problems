import scala.annotation.tailrec

object NinetyNineScalaProblems {

  @tailrec
  def last[T](ls: List[T]): T = ls match {
    case Nil => throw new RuntimeException("no such element")
    case h :: Nil => h
    case h :: tail => last(tail)
  }

  @tailrec
  def penultimate[T](ls: List[T]): T = ls match {
    case _ if ls.size <= 1 => throw new RuntimeException("no such element")
    case beforeLast :: last :: Nil => beforeLast
    case h :: tail => penultimate(tail)
  }

}
