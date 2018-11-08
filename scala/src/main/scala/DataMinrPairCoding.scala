
/**
  * Parse a string that has phone ids and their quantities.
  * IDs can be either for Android or IPhone.
  * Android format is A + 4 characters.
  * IPhone format is I + 3 characters.
  */
object DataMinrPairCoding extends App {
  val exampleString = "Aa1133Iab2Aa112Iac3"

  type Tokens = String

  def tokens(s: String): Stream[Tokens] = {
    if (s.isEmpty) {
      Stream.empty
    } else if(s.startsWith("A")) {
      val (id, rest) = s.splitAt(4)
      id #:: tokens(rest)
    } else if(s.startsWith("I")) {
      val (id, rest) = s.splitAt(3)
      id #:: tokens(rest)
    } else if(s.head.isDigit) {
      val (n, rest) = s.span(_.isDigit)
      n #:: tokens(rest)
    } else throw new Exception("Bad Format of the Input String")
  }

  def parseIntoMap(s: String) = {
    tokens(s).grouped(2).map {
      case Stream(a, b) => (a, b.toInt)
    }
    .toStream
    .groupBy(_._1)
    .mapValues(_.map(_._2).sum)
  }

  println(s"${parseIntoMap(exampleString)}")
}
