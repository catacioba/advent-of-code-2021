package edu.aoc

object Ch07 extends AocSolver {

  override def challenge(): String = "ch07"

  override def solvePart1(input: Seq[String]): Unit = {
    val nums = input.head.split(",").map(_.toInt)

    val min = nums.min
    val max = nums.max

    val optimal = (min to max).map(x => countSimple(nums, x)).min
    println(optimal)
  }

  private def countSimple(nums: Seq[Int], n: Int): Int = {
    nums.map(x => (x - n).abs).sum
  }

  override def solvePart2(input: Seq[String]): Unit = {
    val nums = input.head.split(",").map(_.toInt)

    val min = nums.min
    val max = nums.max

    val optimal = (min to max).map(x => countComplex(nums, x)).min
    println(optimal)
  }

  private def countComplex(nums: Seq[Int], n: Int): Int = {
    nums.map(x => {
      val d = (x - n).abs
      d * (d + 1) / 2
    }).sum
  }
}
