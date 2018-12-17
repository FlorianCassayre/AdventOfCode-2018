package adventofcode.solutions

import adventofcode.Day

object Day17 extends Day(17) {

  case class Point(x: Int, y: Int) {
    def top: Point = copy(y = y - 1)
    def down: Point = copy(y = y + 1)
    def left: Point = copy(x = x - 1)
    def right: Point = copy(x = x + 1)
    def inBounds: Boolean = y >= minY && y <= maxY
  }

  val regex = """(x|y)=(\d+), (x|y)=(\d+)\.\.(\d+)""".r

  val points = lines.flatMap{
    case regex("x", x, "y", ya, yb) => (ya.toInt to yb.toInt).map(y => Point(x.toInt, y))
    case regex("y", y, "x", xa, xb) => (xa.toInt to xb.toInt).map(x => Point(x, y.toInt))
  }.toSet

  val (minY, maxY) = (points.map(_.y).min, points.map(_.y).max)

  def flow(buffer: Set[Point], water: Map[Point, Boolean]): Map[Point, Boolean] = {

    val springs = buffer.filter(_.y <= maxY)

    if(springs.nonEmpty) {
      val below = springs.head.down
      water.get(below) match {
        case Some(false) =>
          flow(springs.tail, water + (springs.head -> false))
        case None if !points.contains(below) =>
          flow(springs.tail + below, water + (springs.head -> false))
        case _ =>
          def walk(f: Point => Point): Seq[Point] = {
            lazy val stream: Stream[Point] = springs.head #:: stream.map(f)
            stream.takeWhile(p => !points.contains(p) && (points.contains(p.down) || water.contains(p.down)))
          }

          val (left, right) = (walk(_.left), walk(_.right))
          val sides = Seq(left.last.left, right.last.right).filter(!points.contains(_))
          val all = left ++ right ++ sides

          val newSprings = springs.tail.filter(p => !all.contains(p))

          if(sides.nonEmpty) {
            flow(newSprings ++ sides, water ++ all.map(_ -> false))
          } else {
            flow(newSprings + springs.head.top, water ++ all.map(_ -> true))
          }
      }
    } else {
      water
    }
  }

  val initial = Point(500, 0)

  val water = flow(Set(initial), Map.empty).filter(_._1.inBounds).values

  override def solutionA = water.size

  override def solutionB = water.count(identity)

  submit()
}
