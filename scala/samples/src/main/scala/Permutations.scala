def permutations[T](xs: Seq[T]): Seq[Seq[T]] =
  if xs.isEmpty then Seq(Seq.empty)
  else
    xs.flatMap { x =>
      permutations(xs.filter(_ != x)).map(x +: _)
    }

@main def runPermutations =
  println(permutations(Seq(1, 2, 3, 4, 5)))
