package adventofcode.solutions

import adventofcode.Day

object Day22 extends Day(22) {

  type Regions = IndexedSeq[IndexedSeq[Int]]

  case class Point(x: Int, y: Int) {
    def adjacent: Set[Point] = Set(copy(x = x - 1), copy(x = x + 1), copy(y = y - 1), copy(y = y + 1))
  }

  val origin = Point(0, 0)

  val regex = """depth: (\d+)
                |target: (\d+),(\d+)""".stripMargin.r

  val (depth, target) = input match {
    case regex(depthStr, targetX, targetY) => (depthStr.toInt, Point(targetX.toInt, targetY.toInt))
  }

  val indices = (0 to target.y).flatMap(y => (0 to target.x).map(x => (x, y)))

  def geologicalIndex(coords: Seq[(Int, Int)], indices: Regions): Regions = {
    if(coords.nonEmpty) {
      val (x, y) = coords.head
      val p = Point(x, y)
      val v = if(p == origin || p == target)
        0
      else if(y == 0)
        x * 16807
      else if(x == 0)
        y * 48271
      else
        indices(y)(x - 1) * indices(y - 1)(x)

      geologicalIndex(coords.tail, indices.updated(y, indices(y).updated(x, (v + depth) % 20183)))
    } else {
      indices
    }
  }

  def areaType(map: Regions): Regions = map.map(_.map(_ % 3))

  def fillRegion(w: Int, h: Int, n: Int): Regions = IndexedSeq.fill(h)(IndexedSeq.fill(w)(n))

  val areas = areaType(geologicalIndex(indices, fillRegion(target.x + 1, target.y + 1, 0)))

  override def solutionA = areas.flatten.sum


  sealed abstract class Equipment {
    def regions: Set[Int]
  }
  case object Neither extends Equipment {
    override def regions: Set[Int] = Set(1, 2)
  }
  case object Torch extends Equipment {
    override def regions: Set[Int] = Set(0, 2)
  }
  case object ClimbingGear extends Equipment {
    override def regions: Set[Int] = Set(0, 1)
  }

  val allEquipments: Set[Equipment] = Set(Neither, Torch, ClimbingGear)

  val biggerArea = areaType(geologicalIndex(indices, fillRegion(target.x + 1 + 7, target.y + 1 + 7, 0)))

  def solve(set: Set[(Point, Int, Equipment)], distances: Map[Equipment, Regions]): Int = {
    def isAllowed(p: Point): Boolean = p.x >= 0 && p.x < biggerArea.head.size && p.y >= 0 && p.y < biggerArea.size

    if(set.nonEmpty) {
      val next = set.flatMap {
        case (position, time, equipment) =>
          val neigbours = position.adjacent.filter(isAllowed)

          neigbours.flatMap{ p =>
            val nextType = biggerArea(p.y)(p.x)

            if(equipment.regions.contains(nextType)) { // No need to switch
              Set((p, time + 1, equipment))
            } else { // Need to switch
              val nextEquipments = allEquipments.filter(_.regions.contains(nextType))
              nextEquipments.map(e => (p, time + 7 + 1, e))
            }
          }.filter{ case (point, t, eq) => t < distances(eq)(point.y)(point.x) }
      }

      val newDistances = set.foldLeft(distances){
        case (map, (point, t, eq)) =>
          val array = map(eq)
          map + (eq -> array.updated(point.y, array(point.y).updated(point.x, Math.min(array(point.y)(point.x), t)))) }
      solve(next, newDistances)
    } else {
      distances.values.map(_(target.y)(target.x)).min
    }
  }

  // solve(Set((origin, 0, Torch)), allEquipments.map(_ -> fillRegion(target.x + 1 + 7, target.y + 1 + 7, Int.MaxValue)).toMap)

  override def solutionB = ???

  submit()
}
