package adventofcode.solutions

import adventofcode.Day

object Day05 extends Day(5) {

  val replacements: Seq[String] = for {
    a <- 'a' to 'z'
    b = a.toUpper
  } yield s"$a$b"

  val allReplacements = replacements ++ replacements.map(_.reverse)

  val evictions: Seq[String] = for {
    a <- 'a' to 'z'
    b = a.toUpper
  } yield s"[$a$b]"

  def computeReaction(s: String): String = {
    val r = allReplacements.foldLeft(s)((acc, replace) => acc.replace(replace, ""))
    if(r == s) r else computeReaction(r)
  }

  override def solutionA = computeReaction(input).length

  override def solutionB = evictions.par.map(evict => input.replaceAll(evict, "")).map(s => computeReaction(s).length).min

  submit()
}
