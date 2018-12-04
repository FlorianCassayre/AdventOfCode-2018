package adventofcode.solutions

import java.text.SimpleDateFormat
import java.util.Calendar

import adventofcode.Day

object Day04 extends Day(4) {

  abstract class Event(val date: Calendar) {
    def minutes: Int = date.get(Calendar.MINUTE)
  }

  case class BeginShift(guard: Int, override val date: Calendar) extends Event(date)
  case class FallsAsleep(override val date: Calendar) extends Event(date)
  case class WakesUp(override val date: Calendar) extends Event(date)

  val format = new SimpleDateFormat("yyyy-MM-dd HH:mm")

  val dateRegex = "\\[(.+)\\] (.+)".r
  val (shiftRegex, sleepRegex, wakesUpRegex) = ("Guard #(\\d+) begins shift".r, "falls asleep".r, "wakes up".r)

  val events: Seq[Event] = lines.map {
    case dateRegex(dateStr, other) =>
      val date = Calendar.getInstance()
      date.setTime(format.parse(dateStr))
      other match {
        case shiftRegex(id) => BeginShift(id.toInt, date)
        case sleepRegex() => FallsAsleep(date)
        case wakesUpRegex() => WakesUp(date)
      }
  }.sortBy(_.date)

  val (_, _, results) = events.scanLeft((0, 0, Map[(Int, Int), Int]())) {
    case ((shift, asleepStart, map), event) =>
      event match {
        case e: BeginShift =>
          (e.guard, asleepStart, map)
        case e: FallsAsleep =>
          (shift, e.minutes, map)
        case e: WakesUp =>
          val updated = (asleepStart until e.minutes).foldLeft(map)((acc, minute) =>
            acc + ((shift, minute) -> (acc.getOrElse((shift, minute), 0) + 1))
          )
          (shift, asleepStart, updated)
      }
  }.last

  override def solutionA = {
    val guard = results.groupBy(_._1._1).mapValues(_.values.sum).maxBy(_._2)._1
    val max = results.filterKeys(_._1 == guard).maxBy(_._2)._1
    max._1 * max._2
  }

  override def solutionB = {
    val max = results.maxBy(_._2)._1
    max._1 * max._2
  }

  submit()
}
