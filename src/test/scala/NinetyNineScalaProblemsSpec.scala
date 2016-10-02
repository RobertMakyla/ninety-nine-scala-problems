import NinetyNineScalaProblems._
import org.scalatest.{FreeSpec, MustMatchers}

class NinetyNineScalaProblemsSpec extends FreeSpec with MustMatchers {

  "P01 (*) Find the last element of a list." - {
    "1 element list" in {
      last(List(1)) mustBe 1
    }
    "n element list" in {
      last(List(1, 2, 3)) mustBe 3
    }
    "0 element list" in {
      intercept[RuntimeException] {
        last(List())
      }.getMessage mustBe "no such element"
    }
  }


}
