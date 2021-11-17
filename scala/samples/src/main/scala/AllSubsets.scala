def allSubsets[T]: Seq[T] => Seq[Seq[T]] =
  case (x +: xs) =>
    val xsSubsets = allSubsets(xs)
    xsSubsets ++ xsSubsets.map(x +: _)

  case Seq() => Seq(Seq.empty)

@main def runAllSubsets =
  println(allSubsets(Seq(1, 2, 3, 4, 5)))
