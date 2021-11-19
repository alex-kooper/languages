def combinations[T](xs: Seq[T], n: Int): Seq[Seq[T]] =
  if n < 0 then throw Exception(s"Invalid length: ${n}")

  if xs.length < n then Seq()
  else if n == 0 then Seq(Seq())
  else
    xs match
      case x +: xs =>
        combinations(xs, n - 1).map(x +: _) ++ combinations(xs, n)
      case Seq() => Seq()

@main def runCombinations =
  println(combinations("abcd", 3))
