
object VestwellCoding extends App {
  val words = Seq(
    "Cat",
    "Tac",
    "Poos",
    "Oops",
    "Soop",
    "Test"
  )

  def makeKey(word: String) = word.toLowerCase.sorted

  def largestAnagramGroup(words: Stream[String]) = words
    .groupBy(makeKey)
    .values
    .maxBy(_.length)

  println(s"Largest Anagram Group ${largestAnagramGroup(words.toStream)}")
}
