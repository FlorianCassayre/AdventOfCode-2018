package adventofcode.solutions

import adventofcode.Day

object Day13 extends Day(13) {

  case class Cart(x: Int, y: Int, vx: Int, vy: Int, state: Int) {
    def tick: Cart = {
      val char = map(y)(x)
      val newState = char match {
        case '/' =>
          copy(vx = -vy, vy = -vx)
        case '\\' =>
          copy(vx = vy, vy = vx)
        case '+' => state match {
          case 0 => copy(vy = -vx, vx = vy, state = 1)
          case 1 => copy(state = 2)
          case 2 => copy(vy = vx, vx = -vy, state = 0)
        }
        case '-' | '|' => this
      }
      newState.copy(x = x + newState.vx, y = y + newState.vy)
    }
    def isCrash(that: Cart): Boolean = x == that.x && y == that.y
    def positionToString: String = s"$x,$y"
  }

  val (map, carts) = {
    val parsed = lines.zipWithIndex.map { case (line, y) =>
      line.zipWithIndex.map { case (char, x) =>
        val (replace, coordinates) = char match {
          case 'v' => ('|', Some(0, 1))
          case '^' => ('|', Some(0, -1))
          case '>' => ('-', Some(1, 0))
          case '<' => ('-', Some(-1, 0))
          case _ => (char, None)
        }
        (replace, coordinates.map { case (vx, vy) => Cart(x, y, vx, vy, 0) })
      }
    }

    (parsed.map(_.map(_._1)), parsed.flatMap(_.flatMap(_._2)))
  }

  def computeTicks(carts: Seq[Cart] = carts, processed: Seq[Cart] = Seq.empty, stopOnCrash: Boolean): String = {
    if(carts.nonEmpty) {
      val ticked = carts.head.tick

      val isCrash = (carts.tail ++ processed).exists(ticked.isCrash)

      val crashFilter: Cart => Boolean = !isCrash || !ticked.isCrash(_)
      val (filteredCarts, filteredProcessed) = (carts.tail.filter(crashFilter), (ticked +: processed).filter(crashFilter))

      val filteredAllCarts = filteredCarts ++ filteredProcessed

      if(stopOnCrash && isCrash)
        ticked.positionToString
      else if(!stopOnCrash && filteredAllCarts.size == 1)
        filteredAllCarts.head.positionToString
      else
        computeTicks(filteredCarts, filteredProcessed, stopOnCrash)
    } else {
      computeTicks(processed.sortBy(c => (c.y, c.x)), stopOnCrash = stopOnCrash)
    }
  }

  override def solutionA = computeTicks(stopOnCrash = true)

  override def solutionB = computeTicks(stopOnCrash = false)

  submit()
}
