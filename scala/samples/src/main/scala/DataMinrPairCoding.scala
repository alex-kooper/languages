
/**
  * Parse a string that has phone ids and their quantities.
  * IDs can be either for Android or IPhone.
  * Android format is A + 4 characters.
  * IPhone format is I + 3 characters.
  * 
  * Bild a map that maps phone IDs into their quantities.
  */

type Tokens = String

def tokens(s: String): Iterator[Tokens] = raw"A\w{3}|I\w{2}|\d+".r.findAllIn(s)

def parseIntoMap(s: String) =
  tokens(s).grouped(2).map {
    case Seq(a, b) => (a, b.toInt)
  }
  .to(LazyList)
  .groupBy(_._1)
  .mapValues(_.map(_._2).sum)
  .toMap

@main def main() =
  val exampleString = "Aa1133Iab2Aa112Iac3"
  println(s"${parseIntoMap(exampleString)}")
