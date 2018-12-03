package adventofcode.solutions

import adventofcode.Day

object Day03 extends Day(3) {

  case class Rectangle(id: Int, x: Int, y: Int, w: Int, h: Int)
  case class Square(id: Int, x: Int, y: Int)

  val regex = "#([0-9]+) @ ([0-9]+),([0-9]+): ([0-9]+)x([0-9]+)".r

  val rects = lines.map{
    case regex(id, x, y, w, h) => Rectangle(id.toInt, x.toInt, y.toInt, w.toInt, h.toInt)
  }

  val squares = for {
    rect <- rects
    x <- 0 until rect.w
    y <- 0 until rect.h
  } yield Square(rect.id, x + rect.x, y + rect.y)

  override def solutionA = squares.groupBy(s => (s.x, s.y)).count(_._2.size > 1)

  override def solutionB = squares.groupBy(s => (s.x, s.y)).filter(_._2.size == 1).values.flatten.groupBy(_.id).find(t => {
    val rect = rects.find(_.id == t._2.head.id).get
    t._2.size == rect.w * rect.h
  }).get._1

  submit()
}
