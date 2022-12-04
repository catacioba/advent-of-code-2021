package edu.aoc

import scala.io.Source

object Ch01 extends AocSolver {

  override def challenge(): String = "ch01"

  override def solvePart1(input: Seq[String]): Unit = {
    val lines = input.map(_.toInt)

    val result = countIncreased(lines)
    println(result)
  }

  override def solvePart2(input: Seq[String]): Unit = {
    val lines = input.map(_.toInt)

    val processedLines = (lines lazyZip lines.drop(1) lazyZip lines.drop(2))
      .map(_ + _ + _)

    val result = countIncreased(processedLines)
    println(result)
  }

  private def countIncreased(data: Seq[Int]): Int = {
    data.zip(data.drop(1))
      .map((num, nextNum) => if nextNum > num then 1 else 0)
      .fold(0)(_ + _)
  }
}
