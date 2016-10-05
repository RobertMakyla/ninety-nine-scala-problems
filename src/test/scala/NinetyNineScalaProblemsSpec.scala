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

  "P04 (*) Find the number of elements of a list." - {
    "0 elements" in {
      NinetyNineScalaProblems.length(Nil) mustBe 0
    }
    "n elements" in {
      NinetyNineScalaProblems.length(List(1, 1, 1)) mustBe 3
    }
  }

  "P05 (*) Reverse a list." - {
    "0 elements" in {
      reverse(Nil) mustBe Nil
    }
    "n elements" in {
      reverse(List(1, 2, 3)) mustBe List(3, 2, 1)
    }
  }

  "P06 (*) Find out whether a list is a palindrome." - {
    case class TestCase(ls: List[Int], expectedResult: Boolean)
    List(
      TestCase(Nil, true),
      TestCase(List(1), true),
      TestCase(List(1, 1), true),
      TestCase(List(1, 2), false),
      TestCase(List(1, 2, 1), true),
      TestCase(List(1, 2, 3), false),
      TestCase(List(1, 2, 2, 1), true),
      TestCase(List(1, 2, 3, 1), false),
      TestCase(List(1, 2, 3, 3, 2, 1), true),
      TestCase(List(1, 2, 3, 3, 1, 1), false)
    ).foreach { test =>
      s"${test.ls} should ${if (test.expectedResult) "" else "NOT "}be detected as palindrome" in {
        isPalindrome(test.ls) mustBe test.expectedResult
      }
    }
  }
  "P07 (**) Flatten a nested list structure." - {
    "0 elements - 1 level" in {
      flatten(Nil) mustBe Nil
    }
    "n elements - 1 level" in {
      flatten(List(1, 2, 3)) mustBe List(1, 2, 3)
    }
    "0 elements - 2 levels" in {
      flatten(List(Nil)) mustBe Nil
    }
    "n elements - 2 levels" in {
      flatten(List(1, 2, 3, List(4))) mustBe List(1, 2, 3, 4)
    }
    "0 elements - 3 levels" in {
      flatten(List(List(Nil))) mustBe Nil
    }
    "1 elements - 3 levels" in {
      flatten(List(List(1))) mustBe List(1)
    }
    "n elements - 3 levels" in {
      flatten(List(1, 2, 3, List(4, List(5)))) mustBe List(1, 2, 3, 4, 5)
    }
  }

  "P08 (**) Eliminate consecutive duplicates of list elements." - {
    "0 elements" in {
      compress(Nil) mustBe Nil
      compressTailRec(Nil) mustBe Nil
    }
    "1 element" in {
      compress(List(1)) mustBe List(1)
      compressTailRec(List(1)) mustBe List(1)
    }
    "same elements" in {
      compress(List(1, 1, 1, 1, 1, 1)) mustBe List(1)
      compressTailRec(List(1, 1, 1, 1, 1, 1)) mustBe List(1)
    }
    "n elements" in {
      compress(List(1, 1, 1, 2, 2, 2, 3, 4, 4)) mustBe List(1, 2, 3, 4)
      compressTailRec(List(1, 1, 1, 2, 2, 2, 3, 4, 4)) mustBe List(1, 2, 3, 4)
    }
    "n elements ending as different elem" in {
      compress(List(1, 1, 1, 2, 2, 2, 3, 4, 4, 5)) mustBe List(1, 2, 3, 4, 5)
      compressTailRec(List(1, 1, 1, 2, 2, 2, 3, 4, 4, 5)) mustBe List(1, 2, 3, 4, 5)
    }
  }
}
