package adventofcode.solutions

import adventofcode.Day

object Day10 extends Day(10) {

  val regex = "position=< *?(-?\\d+), +(-?\\d+)> velocity=< *?(-?\\d+), +(-?\\d+)>".r
  val points = lines.map {
    case regex(sx, sy, svx, svy) => Point(sx.toInt, sy.toInt, svx.toInt, svy.toInt)
  }

  case class Point(x: Double, y: Double, vx: Double, vy: Double) {
  }

  def findMessage(points: Seq[Point], time: Int): (String, Int) = {
    val set = points.map(p => (p.x.toInt, p.y.toInt))
    val aligned: Seq[(Int, Int)] = for {
      (x, y) <- set
      if (1 until 10).forall(i => set.contains((x, y + i)))
    } yield (x, y)

    if(aligned.nonEmpty) {
      val (minX, maxX, minY, maxY) = (points.map(_.x).min.toInt, points.map(_.x).max.toInt, points.map(_.y).min.toInt, points.map(_.y).max.toInt)
      val string = (minY to maxY).map(y => (minX to maxX).map(x => if(set.exists{ case (px, py) => px == x && py == y }) "#" else ".").mkString).mkString("\n")
      (string, time)
    } else {
      findMessage(points.map(p => p.copy(x = p.x + p.vx, y = p.y + p.vy)), time + 1)
    }
  }

  val (message, time) = findMessage(points, 0)

  override def solutionA = message

  override def solutionB = time

  submit()
}
