package me.adav.Exercise

import me.adav.Exercise.PalindromeFinder._
import org.scalatest._

/**
  * Created by adav on 21/12/2015.
  */
class PalindromeFinderSpec extends FlatSpec with Matchers {

  "A Palindrome" should "read the same forwards as backwards" in {
    assert(isPalindrome("a"))
    assert(isPalindrome("aa"))
    assert(isPalindrome("aba"))
    assert(isPalindrome("abba"))
    assert(isPalindrome("cabbac"))
    assert(isPalindrome("dcabbacd"))
    assert(isPalindrome("dcabbacddcabbacd"))
    assert(isPalindrome("dcabbacddcabbacddcabbacddcabbacd"))
    assert(isPalindrome("dcabbacddcabbacddcabbacddcabbacddcabbacddcabbacddcabbacddcabbacd"))
  }

  "A non-palindrome" should "not read the same forwards as backwards" in {
    assert(!isPalindrome("xa"))
    assert(!isPalindrome("xaa"))
    assert(!isPalindrome("xaba"))
    assert(!isPalindrome("xabba"))
    assert(!isPalindrome("xcabbac"))
    assert(!isPalindrome("xdcabbacd"))
    assert(!isPalindrome("xdcabbacddcabbacd"))
    assert(!isPalindrome("xdcabbacddcabbacddcabbacddcabbacd"))
    assert(!isPalindrome("xdcabbacddcabbacddcabbacddcabbacddcabbacddcabbacddcabbacddcabbacd"))
  }

  "A string with n palindrome combinations" should "return a list of length n" in {
    findAllPalindromes("aa").length should be (1)
    findAllPalindromes("aabb").length should be (2)
    findAllPalindromes("aabbcc").length should be (3) //aa,bb,cc

    findAllPalindromes("aba").length should be (1)
    findAllPalindromes("abba").length should be (2)

    findAllPalindromes("racecar").length should be (3)
  }

  "A string with n palindrome distinct combinations" should "return a list of length n" in {
    findAllDistinctPalindromes("aa").length should be (1)
    findAllDistinctPalindromes("aabb").length should be (2)
    findAllDistinctPalindromes("aabbcc").length should be (3) //aa,bb,cc

    findAllDistinctPalindromes("aba").length should be (1)
    findAllDistinctPalindromes("abba").length should be (1)

    findAllDistinctPalindromes("racecar").length should be (1)
  }

  "The McLaren example input string" should "return the given output" in {
    findLongestPalindromes("sqrrqabccbatudefggfedvwhijkllkjihxymnnmzpop", 3).map(_.toString) should contain allOf
      (
        "Text: hijkllkjih, Index: 23, Length: 10",
        "Text: defggfed, Index: 13, Length: 8",
        "Text: abccba, Index: 5, Length: 6"
        )
  }

  "Input string: aoaboobcooocdooood" should "return dooood, coooc, boob" in {
    findLongestPalindromes("aoaboobcooocdooood", 10).map(_.toString) should contain allOf
      (
        "Text: dooood, Index: 12, Length: 6",
        "Text: coooc, Index: 7, Length: 5",
        "Text: boob, Index: 3, Length: 4"
        )
  }

  "Input string: doooodaoacooocboob" should "return dooood, coooc, boob" in {
    findLongestPalindromes("doooodaoacooocboob", 10).map(_.toString) should contain allOf
      (
        "Text: dooood, Index: 0, Length: 6",
        "Text: coooc, Index: 9, Length: 5",
        "Text: boob, Index: 14, Length: 4"
        )
  }

}
