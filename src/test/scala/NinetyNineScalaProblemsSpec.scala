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
    "0 elements" in {isPalindrome(Nil) mustBe true}
    "1 elements" in {isPalindrome(List(1)) mustBe true}
    "2 elements palindrome" in {isPalindrome(List(1,1)) mustBe true}
    "2 elements not palindrome" in {isPalindrome(List(1,2)) mustBe false}
    "3 elements palindrome" in {isPalindrome(List(1,2,1)) mustBe true}
    "3 elements not palindrome" in {isPalindrome(List(1,2,3)) mustBe false}
    "4 elements palindrome" in {isPalindrome(List(1,2,2,1)) mustBe true}
    "4 elements not palindrome" in {isPalindrome(List(1,2,3,1)) mustBe false}
    "5 elements palindrome" in {isPalindrome(List(1,2,3,3,2,1)) mustBe true}
    "5 elements not palindrome" in {isPalindrome(List(1,2,3,3,1,1)) mustBe false}
  }
}
