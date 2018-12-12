package adventofcode.solutions

import adventofcode.Day

object Day12 extends Day(12) {

  case class Rule(replace: Seq[Boolean], by: Boolean)
  case class State(pots: Seq[Boolean], offset: Int) {
    def sum: Int = pots.zipWithIndex.filter(_._1).map(_._2 + offset).sum
    def canonicalized: State = {
      val trimedLeft = pots.dropWhile(!_)
      val trimed = trimedLeft.reverse.dropWhile(!_).reverse
      State(trimed, offset + (pots.size - trimedLeft.size))
    }
    def nextState: State = {
      val filterSize = 5
      val pad = Seq.fill(filterSize - 1)(false)
      val padded = pad ++ pots ++ pad

      val raw = padded.sliding(filterSize, 1).map(g => rules.find(_.replace == g) match {
        case Some(rule) => rule.by
        case None => false
      }).toSeq

      State(raw, offset - filterSize / 2).canonicalized
    }
  }

  def parseLine(s: String): Seq[Boolean] = s.map{
    case '.' => false
    case '#' => true
  }

  val regexHead = "initial state\\: ([\\.#]+)".r
  val regexTail = "([\\.#]+) => ([\\.#])".r
  val initialState = lines.head match {
    case regexHead(init) => parseLine(init)
  }
  val rules = lines.drop(2).map {
    case regexTail(from: String, to) => Rule(parseLine(from), parseLine(to).head)
  }

  def computeWithCycle(state: State, previous: Option[State], i: Long): Long = {
    if(i > 0) {
      val next = state.nextState
      previous.find(_.pots == next.pots) match {
        case Some(value) => value.sum + (state.sum - value.sum) * (i + 1)
        case None => computeWithCycle(next, Some(state), i - 1)
      }
    } else {
      state.sum
    }
  }

  val initial = State(initialState, 0)

  override def solutionA = (0 until 20).foldLeft(initial)((state, _) => state.nextState).sum

  override def solutionB = computeWithCycle(initial, None, 50000000000L)

  submit()
}
