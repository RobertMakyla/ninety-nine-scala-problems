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

  "P02 (*) Find the last but one element of a list." - {
    "0 element list" in {
      intercept[RuntimeException] {
        penultimate(List())
      }.getMessage mustBe "no such element"
    }
    "1 element list" in {
      intercept[RuntimeException] {
        penultimate(List(1))
      }.getMessage mustBe "no such element"
    }
    "2 element list" in {
      penultimate(List(2, 3)) mustBe 2
    }
    "n element list" in {
      penultimate(List(1, 2, 3, 4, 5, 6)) mustBe 5
    }

  }

}
