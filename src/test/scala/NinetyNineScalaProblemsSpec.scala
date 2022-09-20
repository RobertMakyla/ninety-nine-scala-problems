import java.util.NoSuchElementException

import NinetyNineScalaProblems._
import org.scalatest.{FreeSpec, MustMatchers}

class NinetyNineScalaProblemsSpec extends FreeSpec with MustMatchers {

  "higher-order function multiplyBy(Double)" in {
    twenty mustBe 20
  }

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
    case class TestCase[T](hint: String, ls: List[T], expected: List[T])
    List(
      TestCase("0 elements", Nil, Nil),
      TestCase("1 element", List(1), List(1)),
      TestCase("n same elements", List(1, 1, 1, 1, 1, 1), List(1)),
      TestCase("n different elements", List(1, 1, 1, 2, 2, 2, 3, 4, 4), List(1, 2, 3, 4)),
      TestCase("n different elements ending as different elem", List(1, 1, 1, 2, 2, 2, 3, 4, 4, 5), List(1, 2, 3, 4, 5)),
      TestCase(
        "ultimate test",
        List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e),
        List('a, 'b, 'c, 'a, 'd, 'e)
      )
    ).foreach { case TestCase(hint, ls, expected) =>
      hint in {
        compress(ls) mustBe expected
        compressTailRec(ls) mustBe expected
      }
    }
  }

  "P09 (**) Pack consecutive duplicates of list elements into sublists." - {
    case class TestCase[T](hint: String, ls: List[T], expected: List[List[T]])
    List(
      TestCase("0 elements", Nil, Nil),
      TestCase("1 element", List(1), List(List(1))),
      TestCase("2 element - the same", List(1, 1), List(List(1, 1))),
      TestCase("2 element - different", List(1, 2), List(List(1), List(2))),
      TestCase("5 element - different", List(1, 1, 2, 3, 3), List(List(1, 1), List(2), List(3, 3))),
      TestCase("5 element - different but repeating", List(1, 1, 2, 1), List(List(1, 1), List(2), List(1))),
      TestCase(
        "ultimate test",
        List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e),
        List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e))
      )
    ).foreach { case TestCase(hint, ls, expected) =>
      hint in {
        pack(ls) mustBe expected
        packTailRec(ls) mustBe expected
      }
    }

  }

  "P10 (*) Run-length encoding of a list." - {
    "0 elements" in {
      encode(Nil) mustBe Nil
    }
    "1 elements" in {
      encode(List(1)) mustBe List((1, 1))
    }
    "2 elements - the same" in {
      encode(List(1, 1)) mustBe List((2, 1))
    }
    "2 elements- different" in {
      encode(List(9, 9, 8, 7)) mustBe List((2, 9), (1, 8), (1, 7))
    }
    "n elements - ultimate test" in {
      encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) mustBe
        List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))
    }
  }

  "P11 (*) Modified run-length encoding." - {
    "0 elements" - {
      encodeModified(List()) mustBe Nil
    }

    "1 element" - {
      encodeModified(List(10)) mustBe List(10)
    }

    "2 element" - {
      encodeModified(List(10, 10)) mustBe List((2, 10))
    }

    "n element" - {
      encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) mustBe
        List((4, 'a), 'b, (2, 'c), (2, 'a), 'd, (4, 'e))
    }
  }

  "P12 (**) Decode a run-length encoded list." - {
    "0 elements" - {
      decode(List()) mustBe Nil
    }

    "1 element" - {
      decode(List((1, 1))) mustBe List(1)
    }

    "2 element" - {
      decode(List((2, 1))) mustBe List(1, 1)
    }

    "3 elements" - {
      decode(List((2, 9), (1, 8), (1, 7))) mustBe List(9, 9, 8, 7)
    }

    "n element" - {
      decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))) mustBe
        List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
    }
  }

  "P13 (**) Run-length encoding of a list (direct solution)." - {
    "0 elements" in {
      encodeDirect(Nil) mustBe Nil
    }
    "1 elements" in {
      encodeDirect(List(1)) mustBe List((1, 1))
    }
    "2 elements - the same" in {
      encodeDirect(List(1, 1)) mustBe List((2, 1))
    }
    "2 elements- different" in {
      encodeDirect(List(9, 9, 8, 7)) mustBe List((2, 9), (1, 8), (1, 7))
    }
    "n elements - ultimate test" in {
      encodeDirect(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) mustBe
        List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))
    }
  }

  "P14 (*) Duplicate the elements of a list." - {
    "0 elements" in {
      duplicate(Nil) mustBe List()
    }
    "1 elements" in {
      duplicate(List(1)) mustBe List(1, 1)
    }
    "n elements - the same" in {
      duplicate(List(1, 1, 1)) mustBe List(1, 1, 1, 1, 1, 1)
    }
    "n elements - different" in {
      duplicate(List('a, 'b, 'c, 'c, 'd)) mustBe List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd)
    }
  }

  "P15 (**) Duplicate the elements of a list a given number of times." in {
    duplicateN(3)(List('a, 'b, 'c, 'c, 'd)) mustBe
      List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd)
  }

  "P16 (**) Drop every Nth element from a list." - {
    "0 elements" in {
      drop(1, List()) mustBe Nil
    }
    "every 1 elem" in {
      drop(1, List(1, 2, 3)) mustBe Nil
    }
    "every 2 elems" in {
      drop(2, List(1, 2, 3, 4, 5, 6)) mustBe List(1, 3, 5)
    }
    "ultimate" in {
      drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) mustBe List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k)
    }
  }

  "P17 (*) Split a list into two parts." - {
    "0 elements - index 0" in {
      slice(1, List()) mustBe(List(), List())
    }
    "0 elements - index 1" in {
      slice(1, List()) mustBe(List(), List())
    }
    "1 elements" in {
      slice(0, List(1)) mustBe(List(), List(1))
    }
    "n elements" in {
      slice(2, List(0, 1, 2, 3)) mustBe(List(0, 1), List(2, 3))
    }
  }

  "P18 (**) Extract a slice from a list." in {
    slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) mustBe List('d, 'e, 'f, 'g)
  }

  "P19 (**) Rotate a list N places to the left." - {
    "rotate 0" in {
      rotate(0, List(1, 2, 3)) mustBe List(1, 2, 3)
    }

    "rotate 1 forward" in {
      rotate(1, List(1, 2, 3)) mustBe List(2, 3, 1)
    }
    "rotate 1 backward" in {
      rotate(-1, List(1, 2, 3)) mustBe List(3, 1, 2)
    }
    "rotate n forward" in {
      rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) mustBe
        List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c)
    }
    "rotate n backward" in {
      rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) mustBe
        List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i)
    }
  }

  "P20 (*) Remove the Kth element from a list." - {
    "0 elements" in {
      intercept[NoSuchElementException] {
        removeAt(0, List())
      }
    }
    "1 element" in {
      removeAt(0, List(1)) mustBe(List(), 1)
    }
    "n elements" in {
      removeAt(1, List('a, 'b, 'c, 'd)) mustBe(List('a, 'c, 'd), 'b)
    }

  }

  "P21 (*) Insert an element at a given position into a list." - {
    "0 elements" in {
      insertAt('a, 0, List()) mustBe List('a)
    }
    "0 elements - no such element" in {
      intercept[NoSuchElementException] {
        insertAt('a, 1, List())
      }
    }
    "n elements" in {
      insertAt(3, 3, List(0, 1, 2, 4, 5)) mustBe List(0, 1, 2, 3, 4, 5)
    }
    "ultimate test " in {
      insertAt('new, 1, List('a, 'b, 'c, 'd)) mustBe List('a, 'new, 'b, 'c, 'd)
    }
  }

  "P22 (*) Create a list containing all integers within a given range." - {
    "1 elements" in {
      range(1, 1) mustBe List(1)
    }
    "ascending" in {
      range(4, 9) mustBe List(4, 5, 6, 7, 8, 9)
    }
    "descending" in {
      range(5, 3) mustBe List(5, 4, 3)
    }
  }

  "P23 (**) Extract a given number of randomly selected elements from a list." - {
    "list is too small" in {
      intercept[RuntimeException] {
        randomSelect(3, List())
      }.getMessage mustBe "list is too small"
    }
    "n is negative" in {
      intercept[RuntimeException] {
        randomSelect(-1, List(1, 2, 3))
      }.getMessage mustBe "-1 is negative"
    }
    "1 element but nothing to extract" in {
      randomSelect(0, List(1)) mustBe List()
    }
    "1 element" in {
      randomSelect(1, List(3)) mustBe List(3)
    }
    "1/2 elements" in {
      val actual = randomSelect(1, List(3, 4))
      (actual == List(3) || actual == List(4)) mustBe true
    }
    "2/2 elements" in {
      val actual = randomSelect(2, List(3, 4))
      (actual == List(3, 4) || actual == List(4, 3)) mustBe true
    }
    "1/3 elements" in {
      val actual = randomSelect(1, List(3, 4, 5))
      (actual == List(3) || actual == List(4) || actual == List(5)) mustBe true
    }
    "2/3 elements" in {
      val actual = randomSelect(2, List(3, 4, 5))
      (actual == List(3, 4) || actual == List(4, 5) || actual == List(3, 5) ||
        actual == List(4, 3) || actual == List(5, 4) || actual == List(5, 3)
        ) mustBe true
    }

  }

  "P24 (*) Lotto: Draw N different random numbers from the set 1..M." - {
    val max = 5
    val correctValues = 1.to(max)
    "number of elements can't be negative" in {
      intercept[RuntimeException] {
        lotto(-1, max)
      }.getMessage mustBe "-1 is negative"
    }
    "0 elements" in {
      lotto(0, max) mustBe Nil
    }
    "1-5 elements" in {
      1.to(5).foreach { n =>
        val actual = lotto(n, max)
        actual.distinct.size mustBe n
        actual.size mustBe n
        actual.forall(correctValues.contains) mustBe true
      }
    }
  }

  "P25 (*) Generate a random permutation of the elements of a list." - {
    "0 elements" in {
      randomPermute(List()) mustBe Nil
    }
    "1 element" in {
      randomPermute(List(1)) mustBe List(1)
    }
    "2 elements" in {
      val actual = randomPermute(List(1, 2))
      Seq(List(1, 2), List(2, 1)).contains(actual) mustBe true
    }
    "3-5 elements" in {
      3.to(5).foreach { n =>
        val correctValues = 1.to(n).toList
        val actual = randomPermute(correctValues)
        // println("randomPermute(" + correctValues +") => " + actual)
        actual.distinct.size mustBe n
        actual.size mustBe n
        actual.forall(correctValues.contains) mustBe true
      }
    }
  }

  /**
   * All 2 element combination of {a, b, c, d}: 6 possibilities
   * {a, b}, {a, c}, {a, d}, {b, c}, {b, d}, {c, d}.
   */

  "P26 (**) Generate the combinations of K distinct objects chosen from the N elements of a list." - {
    "n = 0" in {
      combinations(0, List('a, 'b, 'c, 'd, 'e, 'f)) mustBe Nil
    }
    "list size = 0" in {
      combinations(1, Nil) mustBe Nil
    }

    "1 elem combinations of 1 elem" in {
      combinations(1, List(1)) mustBe List(List(1))
    }
    "1 elem combinations of 2 elems" in {
      combinations(1, List(1, 2)) mustBe List(List(1), List(2))
    }
    "1 elem combinations of 3 elems" in {
      combinations(1, List(1, 2, 3)) mustBe List(List(1), List(2), List(3))
    }

    "2 elem combinations of 2 elements" in {
      combinations(2, List(1, 2)) mustBe List(List(1, 2))
    }
    "2 elem combinations of 3 elements" in {
      combinations(2, List(1, 2, 3)) mustBe List(List(1, 2), List(1, 3), List(2, 3))
    }
    "2 elem combinations of 4 elements" in {
      combinations(2, List(1, 2, 3, 4)) mustBe List(List(1, 2), List(1, 3), List(1, 4), List(2, 3), List(2, 4), List(3, 4))
    }

    "3 elem combinations of 3 elements" in {
      combinations(3, List(1, 2, 3)) mustBe List(List(1, 2, 3))
    }
    "3 elem combinations of 4 elements" in {
      combinations(3, List(1, 2, 3, 4)) mustBe List(List(1, 2, 3), List(1, 2, 4), List(1, 3, 4), List(2, 3, 4))
    }
    "3 elem combinations of 5 elements" in {
      combinations(3, List(1, 2, 3, 4, 5)) mustBe List(
        List(1, 2, 3),
        List(1, 2, 4),
        List(1, 2, 5),
        List(1, 3, 4),
        List(1, 3, 5),
        List(1, 4, 5),
        List(2, 3, 4),
        List(2, 3, 5),
        List(2, 4, 5),
        List(3, 4, 5)
      )
    }
  }

  "P28 (**) Sorting a list of lists according to length of sublists." in {
    val expected = List(List('o), List('d, 'e), List('d, 'e), List('m, 'n), List('a, 'b, 'c), List('f, 'g, 'h), List('i, 'j, 'k, 'l))
    val actual = lsort(List(List('a, 'b, 'c), List('d, 'e), List('f, 'g, 'h), List('d, 'e), List('i, 'j, 'k, 'l), List('m, 'n), List('o)))
    actual mustBe expected
  }

}