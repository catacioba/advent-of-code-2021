package edu.aoc

import scala.collection.mutable

object Ch06 extends AocSolver {

  override def challenge(): String = "ch06"

  override def solvePart1(input: Seq[String]): Unit = {
    val nums = input.head.split(",").map(_.toInt).toSeq

    var sim = nums
    (0 until 80).foreach(_ => sim = simulate(sim))

    println(sim.length)
  }

  private def simulate(nums: Seq[Int]): Seq[Int] = {
    nums.flatMap(n => {
      if n > 0 then Seq(n - 1) else Seq(6, 8)
    })
  }

  override def solvePart2(input: Seq[String]): Unit = {
    val nums = input.head.split(",").map(_.toInt).toSeq
    val dp = mutable.Map.empty[(Int, Int), Long]

    val cnt = nums.map(x => solveDp(x, 256, dp)).sum
    println(cnt)
  }

  private def solveDp(n: Int, it: Int, dp: mutable.Map[(Int, Int), Long]): Long = {
    val key = (n, it)

    dp.get(key) match {
      case Some(cnt) => cnt
      case None =>
        val cnt = key match {
          case (_, 0) => 1
          case (0, it) => solveDp(6, it - 1, dp) + solveDp(8, it - 1, dp)
          case (n, it) => solveDp(n - 1, it - 1, dp)
        }
        dp.put(key, cnt)
        cnt
    }
  }
}
