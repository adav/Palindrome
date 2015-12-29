package me.adav.Exercise

/**
  * Created by adav on 21/12/2015.
  */
case class Palindrome (text: String, index: Int)  {
  override def toString: String = "Text: " + text + ", Index: " + index + ", Length: " + text.length
}
