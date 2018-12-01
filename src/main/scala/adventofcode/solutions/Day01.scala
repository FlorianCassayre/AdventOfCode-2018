package adventofcode.solutions

import adventofcode.Day

object Day01 extends Day(1) {

  val fs = lines.map(s => {
    val int = s.tail.toInt
    val f: Int => Int = s.head match {
      case '+' => _ + int
      case '-' => _ - int
    }
    f
  })

  override def solutionA = fs.foldLeft(0)((acc, f) => f(acc))

  val stream = Stream.continually(fs).flatten

  def findFirst(sum: Int = 0, set: Set[Int] = Set.empty, stream: Stream[Int => Int] = stream): Int = {
    if(set.contains(sum))
      sum
    else
      findFirst(stream.head(sum), set + sum, stream.tail)
  }

  override def solutionB = findFirst()

  submit()
}
