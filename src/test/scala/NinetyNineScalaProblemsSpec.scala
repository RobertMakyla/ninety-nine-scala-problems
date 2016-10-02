import java.util.NoSuchElementException

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
      intercept[NoSuchElementException] {
        last(List())
      }
    }
  }

  "P02 (*) Find the last but one element of a list." - {
    "0 element list" in {
      intercept[NoSuchElementException] {
        penultimate(List())
      }
    }
    "1 element list" in {
      intercept[NoSuchElementException] {
        penultimate(List(1))
      }
    }
    "2 element list" in {
      penultimate(List(2, 3)) mustBe 2
    }
    "n element list" in {
      penultimate(List(1, 2, 3, 4, 5, 6)) mustBe 5
    }

  }

  "P03 (*) Find the Kth element of a list." - {
    "no such element" in {
      intercept[NoSuchElementException] {
        nth(1, List())
      }
    }
    "index is negative" in {
      intercept[NoSuchElementException] {
        nth(-1, List(1, 2, 3))
      }
    }
    "n elements" in {
      nth(3, List(0, 1, 2, 3, 4, 5)) mustBe 3
    }
    "corner case I: first element" in {
      nth(0, List(2, 3)) mustBe 2
    }
    "corner case II: first element" in {
      nth(1, List(2, 3)) mustBe 3
    }

  }

}
