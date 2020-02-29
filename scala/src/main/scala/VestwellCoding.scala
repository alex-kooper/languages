/*
 Given a list of n string containing letters. Find the size of largest subset of string which are
 anagram of each others.
 An anagram of a string is another string that contains same characters, only the order of characters
 can be different. For example, “abcd” and “dabc” are anagram of each other.
*/

object VestwellCoding extends App {
  val words = Stream(
    "Ant",
    "Magenta",
    "Magnate",
    "Tan",
    "Gnamate",
    "Cat",
    "Tac"
  )

  def makeKey(word: String) = word.toLowerCase.sorted

  def largestAnagramGroup(words: Stream[String]) = words
    .groupBy(makeKey)
    .values
    .maxBy(_.length)

  println(s"Largest Anagram Group ${largestAnagramGroup(words)}")
}
