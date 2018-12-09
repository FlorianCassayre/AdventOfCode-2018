package adventofcode.solutions

import adventofcode.Day

object Day09 extends Day(9) {

  val regex = "(\\d+) players; last marble is worth (\\d+) points".r
  val (players, points) = input match {
    case regex(playersStr, pointsStr) => (playersStr.toInt, pointsStr.toInt)
  }

  class Linked[A](val value: A, var previous: Linked[A], var next: Linked[A])

  object Linked {
    def apply[A](initial: A): Linked[A] = {
      val linked = new Linked(initial, null, null)
      linked.previous = linked
      linked.next = linked
      linked
    }
  }

  def game(circle: Linked[Long] = Linked(0), marble: Int = 1, player: Int = 0, scores: Map[Int, Long] = Map(), last: Int): Long = {
    if(marble <= last) {
      if(marble % 23 != 0) {
        val (previous, next) = (circle, circle.next)
        val insert = new Linked[Long](marble, previous, next)
        previous.next = insert
        next.previous = insert

        game(next, marble + 1, (player + 1) % players, scores, last)
      } else {
        val remove = (0 until 7 + 1).foldLeft(circle)((acc, _) => acc.previous)
        remove.previous.next = remove.next
        remove.next.previous = remove.previous

        game(remove.next.next, marble + 1, (player + 1) % players, scores + (player -> (scores.getOrElse(player, 0L) + remove.value + marble)), last)
      }
    } else
      scores.values.max
  }

  override def solutionA = game(last = points)

  override def solutionB = game(last = points * 100)

  submit()
}
