package adventofcode.solutions

import adventofcode.Day

object Day14 extends Day(14) {

  val inputSeq = input.map(_.asDigit)

  val ten = 10

  def recipes(list: Vector[Int] = Vector(3, 7), a: Int = 0, b: Int = 1, i: Int = input.toInt, reverse: Boolean): Seq[Int] = {
    if(reverse && list.slice(list.size - input.length, list.size) == inputSeq)
      list.dropRight(input.length)
    else if(reverse && list.slice(list.size - input.length - 1, list.size - 1) == inputSeq)
      list.dropRight(input.length + 1)
    else {
      if (reverse || !reverse && list.size - ten <= i) {
        val newList = list ++ (list(a) + list(b)).toString.map(_.asDigit)
        recipes(newList, (a + newList(a) + 1) % newList.size, (b + newList(b) + 1) % newList.size, i, reverse)
      } else {
        list.slice(i, i + ten)
      }
    }
  }

  override def solutionA = recipes(reverse = false).mkString

  override def solutionB = recipes(reverse = true).size

  submit()
}
