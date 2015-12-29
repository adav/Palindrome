package me.adav.Exercise

/**
  * Created by adav on 21/12/2015.
  */
object PalindromeFinder {
////Experiments:
//  def everyPossibleWordForYield(phrase: String) = {
//    for(
//      a <- 0 to phrase.length;
//      b <- 2 + a to phrase.length
//    ) phrase.substring(a, b) match {
//      case x if isPalindrome(x) => println(x)
//      case _ => Nil
//    }
//  }
//
//  def everyPossibleWord(phrase: String) = {
//    for(a <- 0 to phrase.length) {
//      for(b <- 2 + a to phrase.length) {
//        val word = phrase.substring(a, b)
//        if (isPalindrome(word)) word
//      }
//    }
//  }

  implicit class StringExtension(val s: String) {
    def slideWithIndex(size: Int) = s.sliding(size).zipWithIndex
  }

  def isPalindrome(word: String) = word equals word.reverse

  def findAllPalindromes(phrase: String) = {
    (phrase.length to 2 by -1)
      .flatMap { phrase.slideWithIndex(_).filter(x => isPalindrome(x._1)) }
      .map { case (word, index) => Palindrome(word, index) }
 }

  def findAllDistinctPalindromes(phrase: String) = {
    val withInnerPalindromes = findAllPalindromes(phrase)
    val foundPalindromePositions = withInnerPalindromes.map(p => (p.index, p.index + p.text.length - 1) )

    withInnerPalindromes.filterNot { p =>
      foundPalindromePositions.exists(rangeTuple => rangeTuple._1 < p.index && p.index < rangeTuple._2 -1)
    }
  }

  def findLongestPalindromes(phrase: String, n: Int) = findAllDistinctPalindromes(phrase) take n

  def main(args: Array[String]) = {

    assume(args.length > 0, "Include palindromes as a list of arguments.")

    args.foreach { inputString =>
      if (args.length > 1) println("\nInput: " + inputString)

      findLongestPalindromes(inputString, 3)
        .foreach(println)
    }
  }

}
