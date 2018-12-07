package adventofcode.solutions

import adventofcode.Day

object Day07 extends Day(7) {

  case class Edge(from: Char, to: Char)

  val regex = "Step ([A-Z]) must be finished before step ([A-Z]) can begin.".r
  val edges = lines.map {
    case regex(a, b) => Edge(a.head, b.head)
  }

  def topSort(edges: Seq[Edge], set: Set[Char], acc: String): String = {
    if(set.nonEmpty) {
      val n = set.minBy(identity)

      val (newEdges, newSet) = edges.filter(_.from == n).foldLeft((edges, set.filter(_ != n))) {
        case ((tEdges, tSet), edge) =>
          val filtered = tEdges.filter(_ != edge)
          (filtered, if(filtered.forall(_.to != edge.to)) tSet + edge.to else tSet)
      }

      topSort(newEdges, newSet, acc + n)
    } else
      acc
  }

  val initVertices = edges.flatMap(e => Seq(e.from, e.to)).filter(c => edges.forall(_.to != c)).toSet

  override def solutionA = topSort(edges, initVertices, "")



  case class Worker(char: Char, started: Int) {
    def requiredTime: Int = char - 'A' + 1 + 60
    def isFinished(time: Int): Boolean = time - started >= requiredTime
  }

  val nWorkers = 5

  def parTopSort(edges: Seq[Edge], set: Set[Char], acc: String, time: Int, working: Seq[Worker]): (String, Int) = {
    if(set.nonEmpty || working.nonEmpty) {
      val (done, reduced) = working.partition(_.isFinished(time))

      val (edgesDone, setDone) = done.foldLeft((edges, set)){ case ((cEdges, cSet), w) =>
        cEdges.filter(_.from == w.char).foldLeft((cEdges, cSet.filter(_ != w.char))) {
          case ((tEdges, tSet), edge) =>
            val filtered = tEdges.filter(_ != edge)
            (filtered, if(filtered.forall(_.to != edge.to)) tSet + edge.to else tSet)
        }
      }

      val (newSet, newAcc, newWorking) = (working.size until nWorkers).foldLeft((setDone, acc, reduced)) { case (id@(tSet, tAcc, tWorking), _) =>
        if(tSet.nonEmpty) {
          val n = tSet.minBy(identity)
          (tSet.filter(_ != n), tAcc + n, tWorking :+ Worker(n, time))
        } else
          id
      }

      parTopSort(edgesDone, newSet, newAcc, time + 1, newWorking)
    } else
      (acc, time - 1 - 1)
  }

  override def solutionB = parTopSort(edges, initVertices, "", 0, Seq())._2

  submit()
}
