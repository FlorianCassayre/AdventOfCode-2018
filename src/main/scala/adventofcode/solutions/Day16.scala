package adventofcode.solutions

import adventofcode.Day

object Day16 extends Day(16) {

  val nRegisters = 4

  type Registers = IndexedSeq[Int]

  case class ProgramInstruction(op: Int, a: Int, b: Int, c: Int)
  object ProgramInstruction {
    def apply(seq: IndexedSeq[Int]): ProgramInstruction = seq match {
      case Seq(op, a, b, c) => ProgramInstruction(op, a, b, c)
    }
  }

  case class Sample(input: Registers, instruction: ProgramInstruction, output: Registers)

  type RegisterOperation = (ProgramInstruction, Registers) => Registers

  def regop(f: (Int, Int) => Int, comparison: Boolean = false, set: Boolean = false): Seq[RegisterOperation] = {
    val ops: Seq[RegisterOperation] = Seq((i, rg) => rg.updated(i.c, f(rg(i.a), rg(i.b))), (i, rg) => rg.updated(i.c, if(set) i.a else f(rg(i.a), i.b)))
    val additional: Seq[RegisterOperation] = if(comparison) Seq((i, rg) => rg.updated(i.c, f(i.a, rg(i.b)))) else Seq.empty
    ops ++ additional
  }

  val instructions: Seq[RegisterOperation] = Seq(
    regop(_ + _), // add
    regop(_ * _), // mul
    regop(_ & _), // band
    regop(_ | _), // bor
    regop((a, _) => a, set = true), // set
    regop((a, b) => if(a > b) 1 else 0, comparison = true), // gt
    regop((a, b) => if(a == b) 1 else 0, comparison = true), // eq
  ).flatten

  val regex = """Before: +\[([\d ,]+)\]
                |([\d ]+)
                |After: +\[([\d ,]+)\]""".stripMargin.r
  val (samples, program) = {
    val split = input.split("\n{4}")
    val operations = split(0).split("\n{2}").map {
      case regex(before, instruction, after) =>
        Sample(before.split(", ").map(_.toInt).toIndexedSeq, ProgramInstruction(instruction.split(" ").map(_.toInt)), after.split(", ").map(_.toInt).toIndexedSeq)
    }
    (operations, split(1).split("\n").map(_.split(" ").map(_.toInt)).map(ProgramInstruction(_)).toSeq)
  }

  override def solutionA = samples.count(sample => instructions.count(_(sample.instruction, sample.input) == sample.output) >= 3)

  def decipher(unknown: Set[RegisterOperation], known: Map[Int, RegisterOperation]): Map[Int, RegisterOperation] = {
    if(unknown.nonEmpty) {
      val discovered = samples.filter(sample => known.keys.forall(_ != sample.instruction.op))
        .map(sample =>
          sample.instruction.op -> unknown.filter(_(sample.instruction, sample.input) == sample.output)
        ).filter(_._2.size == 1).toMap.mapValues(_.head)

      decipher(unknown -- discovered.values, known ++ discovered)
    } else {
      known
    }
  }

  val opCodes = decipher(instructions.toSet, Map.empty)

  override def solutionB = program.foldLeft(IndexedSeq.fill(nRegisters)(0))((registers, instruction) => opCodes(instruction.op)(instruction, registers))(0)

  submit()
}
