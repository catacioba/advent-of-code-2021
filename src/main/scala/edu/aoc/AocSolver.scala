package edu.aoc

import scala.io.{BufferedSource, Source}

private trait AocSolver {

  def challenge(): String

  def solvePart1(input: Seq[String]): Unit

  def solvePart2(input: Seq[String]): Unit

  def solve(): Unit = {
    val testInput = readTestInput()
    val finalInput = readFinalInput()

    println(s"For challenge ${challenge()}:")
    println("* Part 1")
    println("** Test:")
    solvePart1(testInput)
    println("** Final:")
    solvePart1(finalInput)
    println()
    println("* Part 2")
    println("** Test:")
    solvePart2(testInput)
    println("** Final:")
    solvePart2(finalInput)
  }

  private def readTestInput(): Seq[String] =
    readFromFile(s"data/${challenge()}/test.txt")

  private def readFinalInput(): Seq[String] = readFromFile(
    s"data/${challenge()}/final.txt"
  )

  private def readFromFile(filename: String): Seq[String] = {
    val bs = Source.fromFile(filename)
    bs.getLines().toSeq
  }
}
