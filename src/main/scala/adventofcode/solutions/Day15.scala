package adventofcode.solutions

import adventofcode.Day

object Day15 extends Day(15) {

  case class Point(x: Int, y: Int) {
    def reachable: Seq[Point] = Seq(copy(y = y - 1), copy(x = x - 1), copy(x = x + 1), copy(y = y + 1))
    def order: (Int, Int) = (y, x)
  }
  case class Creature(position: Point, isGoblin: Boolean, health: Int = 200)

  val (map, entities) = {
    val parsed = lines.zipWithIndex.map { case (line, y) =>
      line.zipWithIndex.map { case (value, x) =>
        value match {
          case '#' => (false, None)
          case '.' => (true, None)
          case 'G' => (true, Some(Creature(Point(x, y), isGoblin = true)))
          case 'E' => (true, Some(Creature(Point(x, y), isGoblin = false)))
        }
      }
    }
    (parsed.map(_.map(_._1)), parsed.flatMap(_.flatMap(_._2)))
  }

  def turn(waiting: Seq[Creature] = Seq.empty, played: Seq[Creature] = entities, rounds: Int = 0, elvesAttack: Int = 3): (Int, Boolean) = {

    val creatures = waiting ++ played

    def noWall(p: Point): Boolean = p.x >= 0 && p.x < map.head.size && p.y >= 0 && p.y < map.size && map(p.y)(p.x)
    def canWalk(p: Point): Boolean = noWall(p) && creatures.forall(_.position != p)

    def distance(from: Point, to: Point): Option[Int] = {
      def dfs(sources: Set[Point], visited: Set[Point], d: Int): Option[Int] = {
        if(sources.nonEmpty)
          if(sources.contains(to))
            Some(d)
          else {
            val newVisited = visited ++ sources
            dfs(sources.flatMap(_.reachable).filter(canWalk).diff(newVisited), newVisited, d + 1)
          }
        else
          None
      }
      dfs(Set(from), Set.empty, 0)
    }

    val end = {
      val (goblin, elves) = creatures.partition(_.isGoblin)
      goblin.isEmpty || elves.isEmpty
    }

    if(end) {
      val outcome = creatures.map(_.health).sum * (rounds - 1)

      (outcome, entities.count(!_.isGoblin) == creatures.count(!_.isGoblin))
    } else waiting match {
      case that +: tail =>
        val orientations = that.position.reachable.filter(noWall)

        val currentEnemies = creatures.filter(_ != that).filter(_.isGoblin != that.isGoblin)

        val newThat =
          if(!currentEnemies.exists(e => orientations.contains(e.position))) {
            val targets = orientations.filter(canWalk).map { o =>
              val distances = currentEnemies
                .flatMap(_.position.reachable).filter(canWalk)
                .flatMap(p => distance(o, p))
              o -> (if(distances.nonEmpty) Some(distances.min) else None)
            }

            val distances = targets.flatMap(_._2)
            if(distances.nonEmpty) {
              val min = distances.min
              val goto = targets.filter(t => t._2.isDefined && t._2.get == min).minBy(_._1.order)._1
              that.copy(position = goto)
            } else {
              that
            }
          } else that

        val enemies = creatures.filter(_ != newThat).filter(_.isGoblin != that.isGoblin).sortBy(c => (c.health, c.position.order))
        val maybeTarget = enemies.find(e => newThat.position.reachable.filter(noWall).contains(e.position))

        val (newWaiting, newPlayed) = maybeTarget match {
          case Some(target) =>
            val attackPower = if(newThat.isGoblin) 3 else elvesAttack

            val victimUpdated = target.copy(health = target.health - attackPower)

            def update(seq: Seq[Creature]): Seq[Creature] = (seq.indexOf(target) match {
              case -1 => seq
              case i => seq.updated(i, victimUpdated)
            }).filter(_.health > 0)

            (update(tail), update(newThat +: played))
          case None =>
            (tail, newThat +: played)
        }

        turn(newWaiting, newPlayed, rounds, elvesAttack)
      case Seq() =>
        turn(played.sortBy(_.position.order), Seq.empty, rounds + 1, elvesAttack)
    }
  }

  override def solutionA = turn()._1

  override def solutionB = Stream.from(4).map(i => turn(elvesAttack = i)).find(_._2).get._1

  submit()
}
