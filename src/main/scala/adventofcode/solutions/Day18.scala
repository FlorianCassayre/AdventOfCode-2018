package adventofcode.solutions

import adventofcode.Day

object Day18 extends Day(18){

  type Collection = IndexedSeq[IndexedSeq[State]]

  sealed trait State
  case object Open extends State
  case object Tree extends State
  case object Lumberyard extends State

  val states = Set(Open, Tree, Lumberyard)

  val initial = lines.map(_.map {
    case '.' => Open
    case '|' => Tree
    case '#' => Lumberyard
  })

  val (width, height) = (initial.head.size, initial.size)

  def next(map: Collection): Collection =
    map.zipWithIndex.map { case (line, y) =>
      line.zipWithIndex.map { case (state, x) =>
        val adjacent = for {
          y1 <- -1 to 1
          x1 <- -1 to 1
          if x1 != 0 || y1 != 0
        } yield (x + x1, y + y1)
        val adjacentInside = adjacent
          .filter { case (x1, y1) => x1 >= 0 && x1 < width && y1 >= 0 && y1 < height }
          .map { case (x1, y1) => map(y1)(x1) }
        val counts = states.map(state => state -> adjacentInside.count(state == _)).toMap
        state match {
          case Open if counts(Tree) >= 3 => Tree
          case Tree if counts(Lumberyard) >= 3 => Lumberyard
          case Lumberyard if !(counts(Lumberyard) >= 1 && counts(Tree) >= 1) => Open
          case _ => state
        }
      }
    }

  lazy val configurations: Stream[Collection] = initial #:: configurations.map(next)

  def value(collection: Collection): Int = {
    val flat = collection.flatten
    flat.count(Tree == _) * flat.count(Lumberyard == _)
  }

  override def solutionA = value(configurations(10))

  def withCycleSimplification(stream: Stream[Collection], reached: Seq[Collection], i: Int): Collection = {
    if(i > 0)
      reached.indexOf(stream.head) match {
        case -1 => withCycleSimplification(stream.tail, stream.head +: reached, i - 1)
        case n => reached(n - i % (n + 1))
      }
    else
      stream.head
  }

  override def solutionB = value(withCycleSimplification(configurations, Seq.empty, 1000000000))

  submit()
}
