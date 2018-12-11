package adventofcode.solutions

import adventofcode.Day

object Day11 extends Day(11) {

  val serialNumber = input.toInt

  val size = 300
  val powers: IndexedSeq[IndexedSeq[Int]] = (0 until size).map(y => (0 until size).map(x => powerLevel(x, y)))

  def powerLevel(x: Int, y: Int): Int = {
    val rackId = x + 10
    val power = (rackId * y + serialNumber) * rackId
    power.toString.reverse(2).asDigit - 5
  }

  def maxSquareSubArray(k: Int): (Int, Int, Int, Int) = {
    val subArrays = Array.ofDim[Int](size, size)

    for(j <- 0 until size) {
      var sum = (0 until k).map(powers(_)(j)).sum
      subArrays(0)(j) = sum
      for(i <- 1 until size - k + 1) {
        sum += powers(i + k - 1)(j) - powers(i - 1)(j)
        subArrays(i)(j) = sum
      }
    }

    var maxSum = Int.MinValue
    var (mx, my) = (0, 0)
    for(i <- 0 until size) {
      var sum = (0 until k).map(subArrays(i)(_)).sum
      if(sum >= maxSum) {
        maxSum = sum
        mx = 0
        my = i
      }

      for(j <- 1 until size - k + 1) {
        sum += subArrays(i)(j + k - 1) - subArrays(i)(j - 1)
        if(sum > maxSum) {
          maxSum = sum
          mx = j
          my = i
        }
      }
    }

    (mx, my, k, maxSum)
  }

  override def solutionA = {
    val (x, y, _, _) = maxSquareSubArray(3)
    s"$x,$y"
  }

  override def solutionB = {
    val (x, y, k, _) = (1 until size).map(k => maxSquareSubArray(k)).maxBy(_._4)
    s"$x,$y,$k"
  }

  submit()
}
