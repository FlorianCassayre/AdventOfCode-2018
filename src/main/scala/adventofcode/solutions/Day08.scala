package adventofcode.solutions

import adventofcode.Day

object Day08 extends Day(8) {

  val data = input.split(" ").map(_.toInt)

  def parse(seq: Seq[Int], f: (Seq[Int], Seq[Int]) => Int): (Seq[Int], Int) = {
    val (nChildren, nMetadata) = (seq(0), seq(1))
    val (right, values) = (0 until nChildren).foldLeft((seq.drop(2), Seq[Int]())){ case ((left, acc), _) =>
      val (residue, value) = parse(left, f)
      (residue, acc :+ value)
    }
    val (metadata, other) = right.splitAt(nMetadata)
    (other, if(nChildren > 0) f(values, metadata) else values.sum + metadata.sum)
  }

  override def solutionA = parse(data, _.sum + _.sum)._2

  override def solutionB = parse(data, (a, b) => b.filter(_ <= a.size).map(_ - 1).map(a).sum)._2

  submit()
}
