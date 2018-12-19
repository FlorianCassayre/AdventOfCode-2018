package adventofcode.solutions

import adventofcode.Day

object Day19 extends Day(19) {

  val nRegisters = 6

  type Registers = IndexedSeq[Int]

  val (ipRegex, instructionRegex) = ("#ip (\\d+)".r, "([a-z]+) (\\d+) (\\d+) (\\d+)".r)

  val ip = lines.head match {
    case ipRegex(reg) => reg.toInt
  }
  val program = lines.tail.map {
    case instructionRegex(opCode, as, bs, cs) =>
      def b2i(b: Boolean): Int = if(b) 1 else 0
      val (a, b, c) = (as.toInt, bs.toInt, cs.toInt)
      val op: Registers => Int = opCode match {
        case "addr" => r => r(a) + r(b)
        case "addi" => r => r(a) + b
        case "mulr" => r => r(a) * r(b)
        case "muli" => r => r(a) * b
        case "banr" => r => r(a) & r(b)
        case "bani" => r => r(a) & b
        case "borr" => r => r(a) | r(b)
        case "bori" => r => r(a) | b
        case "setr" => r => r(a)
        case "seti" => r => a
        case "gtir" => r => b2i(a > r(b))
        case "gtri" => r => b2i(r(a) > b)
        case "gtrr" => r => b2i(r(a) > r(b))
        case "eqir" => r => b2i(a == r(b))
        case "eqri" => r => b2i(r(a) == b)
        case "eqrr" => r => b2i(r(a) == r(b))
      }
      (r: Registers) => r.updated(c, op(r))
  }

  def next(r: Registers): Option[Registers] = {
    val i = r(ip)
    if(i < program.size) {
      val executed = program(i)(r)
      val incremented = executed.updated(ip, executed(ip) + 1)
      Some(incremented)
    } else {
      None
    }
  }

  def run(r: Registers): Registers = {
    next(r) match {
      case Some(update) => run(update)
      case None => r
    }
  }

  val initial: Registers = IndexedSeq.fill(nRegisters)(0)

  override def solutionA = run(initial)(0)

  // run(initial.updated(0, 1))(0)
  // nope.

  lazy val stream: Stream[Registers] = initial.updated(0, 1) #:: stream.flatMap(next)

  val magic = stream.find(_(ip) == 1).get(3)

  override def solutionB = (1 to magic).filter(magic % _ == 0).sum

  submit()
}
