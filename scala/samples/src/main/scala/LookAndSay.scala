// Facebook interview sample "Look and Say"
//
// Implement a function that outputs the Look and Say sequence:
//
// 1
// 11
// 21
// 1211
// 111221
// 312211
// 13112221
// 1113213211
// 31131211131221
// 13211311123113112211

def next(s: String): String =
  if s.isEmpty then ""
  else
    val (prefix, rest) = s.span(_ == s(0))
    s"${prefix.length}${s(0)}${next(rest)}"

def lookAndSay = LazyList.iterate("1")(next)

@main def printLookAndSay() =
  println(lookAndSay.take(10).mkString("\n"))
