// Arrays: Merge overlapping intervals

// You are given an array (list) of interval pairs as input where each interval
// has a start and end timestamp. The input array is sorted by starting
// timestamps. You are required to merge overlapping intervals and return a new
// output array.

// Consider the input array below.
// Intervals (1, 5), (3, 7), (4, 6), (6, 8) are overlapping so they should be
// merged to one big interval (1, 8).

// Similarly, intervals (10, 12) and (12, 15) are also overlapping and should
// be merged to (10, 15).

import scala.math.{max, min}

type Interval = (Int, Int)

def addInterval(xs: Seq[Interval], x: Interval): Seq[Interval] = (xs, x) match
  case ((l1, r1) +: rest, (l2, r2)) =>
    if l2 <= r1 then (l1, max(r1, r2)) +: rest
    else (l2, r2) +: (l1, r1) +: rest
  case (Seq(), (l, r)) =>
    Seq((l, r))

def mergeIntervals(xs: Seq[(Int, Int)]): Seq[(Int, Int)] =
  xs.map((l, r) => (min(l, r), max(l, r))) // make sure left is less than right
    .sortBy((l, r) =>
      l
    ) // Sort them, just in case, even though in requirements they are sorted
    .foldLeft(Seq.empty)(addInterval)
    .reverse

@main def runMergeIntervals =
  val ints = Seq((1, 5), (3, 1), (4, 6), (8, 6), (10, 12), (11, 15))
  println(s"Merge Intervals on: ${ints}\n${mergeIntervals(ints)}")
