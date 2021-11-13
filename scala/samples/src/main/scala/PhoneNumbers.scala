
object PhoneNumbers extends App {
  val digitsToChars = Map(
      '0' -> "0",
      '1' -> "1",
      '2' -> "abc",
      '3' -> "def",
      '4' -> "ghi",
      '5' -> "jkl",
      '6' -> "mno",
      '7' -> "pqrs",
      '8' -> "tuv",
      '9' -> "wxyz"
  )

  def phoneNumberToWords(s: String): Seq[String] = {
    if(s.isEmpty) {
      Seq("")
    } else for {
      tail <- phoneNumberToWords(s.drop(1))
      head <- digitsToChars(s.head)
    } yield head + tail
  }

  def phoneNumberToWordsFormatted(s: String): String = phoneNumberToWords(s).sorted.mkString(",")

  println(s"Result is:\n${phoneNumberToWordsFormatted("2453270")}")
}
