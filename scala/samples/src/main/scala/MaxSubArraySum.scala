
// Kadane's algorithm implementation that just calculates the sum
def maxSubListSum(xs: Iterable[Int]): Int =
  xs.tail.view
    .scanLeft(xs.head)((accum, v) => v.max(accum + v))
    .max


// Below is Kadane's alhorithm implementation that returns largest sum
// contiguous sub-list starting and ending indexes plus the sum

case class SubList(start: Int, end: Int, sum: Int)

def maxSubListSumInfo(xs: Iterable[Int]): SubList =
  xs.tail.view
    .scanLeft(SubList(0, 1, xs.head)) { case (SubList(start, end, sum), v) =>
      if sum + v > v then SubList(start, end + 1, sum + v)
      else SubList(end, end + 1, v)
    }
    .maxBy(_.sum)

@main def runMaxSubListSum =
  val input = List(-2, -3, 4, -1, -2, 1, 5, -3)
  val result = maxSubListSumInfo(input)

  println(s"Input list: ${input.mkString(", ")}")
  println(s"Maximum contiguous sub-list sum: ${maxSubListSum(input)}")
  println(
    s"Maximum contiguous sub-list indexes [${result.start} : ${result.end}], sum = ${result.sum}"
  )
