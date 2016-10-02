import scala.annotation.tailrec

object NinetyNineScalaProblems {

  @tailrec
  def last[T](ls: List[T]): T = ls match {
    case Nil => throw new RuntimeException("no such element")
    case h :: Nil => h
    case h :: tail => last(tail)
  }

}
