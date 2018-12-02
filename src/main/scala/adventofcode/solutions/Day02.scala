package adventofcode.solutions

import adventofcode.Day

object Day02 extends Day(2) {

  override def solutionA = lines.flatMap(_.groupBy(identity).mapValues(_.length).values.toSet)
    .groupBy(identity).filterKeys(_ != 1).mapValues(_.size).values.product

  val candidates = for {
    a <- lines
    b <- lines
    filtered = a.zip(b).filter(t => t._1 == t._2)
    if filtered.length == a.length - 1
  } yield a.intersect(b)

  override def solutionB = candidates.head

  submit()
}
