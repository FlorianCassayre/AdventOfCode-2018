package adventofcode.solutions

import adventofcode.Day

object Day06 extends Day(6) {

  case class Point(x: Int, y: Int) {
    def distance(that: Point): Int = Math.abs(x - that.x) + Math.abs(y - that.y)
  }

  val regex = "(\\d+), (\\d+)".r
  val parsed = lines.map { case regex(x, y) => Point(x.toInt, y.toInt) }

  val (minX, maxX, minY, maxY) = (parsed.map(_.x).min, parsed.map(_.x).max, parsed.map(_.y).min, parsed.map(_.y).max)

  def computeAreas(rangeX: Range, rangeY: Range): Seq[(Point, Int)] = {
    val areas = for {
      y <- rangeX
      x <- rangeY
      p = Point(x, y)
      min = parsed.map(p.distance).min
      closest = parsed.filter(p.distance(_) == min)
    } yield closest match {
      case Seq(one) => Some(one)
      case _ => None
    }
    areas.flatten.groupBy(identity).mapValues(_.size).toSeq
  }

  override def solutionA =
    computeAreas(minX to maxX, minY to maxY).zip(computeAreas((minX - 1) to (maxX + 1), (minY - 1) to (maxY + 1)))
      .filter(t => t._1._2 == t._2._2).map(_._1).minBy(-_._2)._2

  val region = for {
    y <- minY to maxY
    x <- minX to maxX
    p = Point(x, y)
    if parsed.map(p.distance).sum < 10000
  } yield 1

  override def solutionB = region.sum

  submit()
}
