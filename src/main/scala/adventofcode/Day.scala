package adventofcode

import java.io.PrintWriter

import scala.io.Source
import scala.util.{Failure, Success, Try}

abstract class Day(day: Int) extends App {

  sealed abstract class SubProblem(val name: String)
  case object A extends SubProblem("A")
  case object B extends SubProblem("B")

  require(day >= 1 && day <= 25, "Invalid day number")

  protected val lineSeparator: String = "\n"

  private val formatted = "%02d".format(day)

  private def readInput(): String = Source.fromFile(s"input/$formatted.txt").getLines().mkString(lineSeparator)

  private def writeOutput(output: String, subProblem: SubProblem): Unit = {
    val sub = subProblem.name
    new PrintWriter(s"output/$formatted$sub.txt") {
      write(output)
      close()
    }
  }

  protected val input: String = readInput()
  protected val lines: IndexedSeq[String] = input.split(lineSeparator).toIndexedSeq


  def solutionA: Any
  def solutionB: Any


  // To be called at the end

  private var submitted: Boolean = false

  protected final def submit(): Unit = {
    assert(!submitted, "Solution has already been submitted")
    submitted = true

    submitSolution(solutionA, A)
    submitSolution(solutionB, B)
  }

  private def submitSolution(function: => Any, subProblem: SubProblem): Unit = {
    Try(function.toString) match {
      case Success(string) =>
        writeOutput(string, subProblem)
        println(string)
        println()
      case Failure(_: NotImplementedError) =>
        println(s"(ignored solution ${subProblem.name})")
        println()
      case Failure(exception) =>
        throw new RuntimeException(exception)
    }
  }

}
