def combinations[T](xs: Seq[T]): Seq[Seq[T]] =
  if xs.isEmpty then Seq(Seq.empty)
  else
    xs.flatMap { x =>
      combinations(xs.filter(_ != x)).map(x +: _)
    }

@main def runCombinations =
  println(combinations(Seq(1, 2, 3, 4, 5)))
