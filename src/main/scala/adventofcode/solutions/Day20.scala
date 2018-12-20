package adventofcode.solutions

import adventofcode.Day

object Day20 extends Day(20) {

  case class Point(x: Int, y: Int)
  case class Edge(p1: Point, p2: Point) {
    def swap: Edge = Edge(p2, p1)
  }

  type Direction = Point => Point

  sealed abstract class Expression
  case class StraightExpression(directions: Seq[Direction]) extends Expression
  case class CompositeExpression(expressions: Seq[Expression]) extends Expression
  case class BranchingExpression(branches: Seq[Expression]) extends Expression

  val inputRegex = """\^([NESW\(\)\|]*)\$""".r

  val expression = {
    val (_, _, seq) = input match {
      case inputRegex(in) => in.foldLeft((Seq.empty[Direction], Seq.empty[Seq[Expression]], Seq[Seq[Expression]](Seq.empty))) {
        case ((straight, branches, linear), c) =>
          lazy val composite = linear.head :+ StraightExpression(straight)
          c match {
            case '(' =>
              (Seq.empty, Seq.empty +: branches, Seq.empty +: composite +: linear.tail)
            case ')' =>
              val branch = BranchingExpression(branches.head :+ CompositeExpression(composite))
              (Seq.empty, branches.tail, (linear.tail.head :+ branch) +: linear.tail.tail)
            case '|' =>
              (Seq.empty, (branches.head :+ CompositeExpression(composite)) +: branches.tail, Seq.empty +: linear.tail)
            case _ =>
              val direction: Direction = c match {
                case 'N' => p => p.copy(y = p.y - 1)
                case 'E' => p => p.copy(x = p.x + 1)
                case 'S' => p => p.copy(y = p.y + 1)
                case 'W' => p => p.copy(x = p.x - 1)
              }
              (straight :+ direction, branches, linear)
          }
      }
    }
    CompositeExpression(seq.head)
  }

  def reachable(from: Point, expression: Expression): (Set[Point], Set[Edge]) = {
    expression match {
      case StraightExpression(directions) =>
        val path = directions.scanLeft(from)((point, direction) => direction(point))
        (Set(path.last), path.zip(path.tail).map { case (p1, p2) => Edge(p1, p2) }.toSet)
      case CompositeExpression(expressions) =>
        expressions.foldLeft((Set(from), Set.empty[Edge])) {
          case ((points, edges), subExpression) =>
            val (newPoints, newEdges) = points.toSeq.map(reachable(_, subExpression)).unzip
            (newPoints.foldLeft(Set.empty[Point])(_ ++ _), edges ++ newEdges.foldLeft(Set.empty[Edge])(_ ++ _))
        }
      case BranchingExpression(expressions) =>
        expressions.foldLeft((Set.empty[Point], Set.empty[Edge])) {
          case ((points, edges), branch) =>
            val (newPoints, newEdges) = reachable(from, branch)
            (points ++ newPoints, edges ++ newEdges)
        }
    }
  }

  val initial = Point(0, 0)

  val (_, finalEdges) = reachable(initial, expression)
  val graph = finalEdges.flatMap(e => Set(e, e.swap)).groupBy(_.p1).mapValues(_.map(_.p2).toSet)

  def doorsDistance(from: Set[Point], distances: Map[Point, Int], depth: Int): Map[Point, Int] = {
    if(from.nonEmpty) {
      val newReached = distances ++ from.diff(distances.keySet).map(_ -> depth).toMap
      doorsDistance(from.flatMap(graph.getOrElse(_, Set.empty)).diff(newReached.keySet), newReached, depth + 1)
    }
    else
      distances
  }

  val distances = doorsDistance(Set(initial), Map.empty, 0).values

  override def solutionA = distances.max

  override def solutionB = distances.count(_ >= 1000)

  submit()
}
