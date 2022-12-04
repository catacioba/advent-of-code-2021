package edu.aoc

import scala.annotation.tailrec
import scala.collection.mutable

object Ch04 extends AocSolver {

  override def challenge(): String = "ch04"

  class Board(val numbers: IndexedSeq[IndexedSeq[Int]]) {
    private val numbersExtracted: mutable.HashSet[Int] = mutable.HashSet.empty

    def markNumber(n: Int): Boolean = {
      numbersExtracted.add(n)
      checkRows || checkColumns
    }

    def checkRows: Boolean = numbers.exists(r => r.count(numbersExtracted.contains) == 5)

    def checkColumns: Boolean = (0 until 5).exists(c => numbers.map(r => r(c)).count(numbersExtracted.contains) == 5)

    def unmarkedSum: Int = {
      numbers.flatten.filterNot(numbersExtracted.contains).sum
    }
  }

  override def solvePart1(input: Seq[String]): Unit = {
    val (numbers, boards) = parseInput(input)

    println(playToWin(numbers, 0, boards))
  }

  @tailrec
  private def playToWin(numbers: Seq[Int], idx: Int, boards: List[Board]): Int = {
    val n = numbers(idx)
    boards
      .flatMap(b => if b.markNumber(n) then Some(b.unmarkedSum * n) else None)
      .headOption
    match {
      case Some(s) => s
      case None => playToWin(numbers, idx + 1, boards)
    }
  }

  override def solvePart2(input: Seq[String]): Unit = {
    val (numbers, boards) = parseInput(input)

    println(playToLose(numbers, 0, boards))
  }

  @tailrec
  private def playToLose(numbers: Seq[Int], idx: Int, boards: List[Board]): Int = {
    val n = numbers(idx)
    val losingBoards = boards
      .filterNot(b => b.markNumber(n))

    if losingBoards.length > 1 then playToLose(numbers, idx + 1, losingBoards)
    else playToWin(numbers, idx + 1, losingBoards)
  }

  private def parseInput(input: Seq[String]): (Seq[Int], List[Board]) = {
    val data = input.toIndexedSeq

    val numbers = data(0).split(",").map(_.toInt)
    val boards = parseBoards(data, 2)

    (numbers, boards)
  }

  private def parseBoards(data: IndexedSeq[String], row: Int): List[Board] = {
    if row < data.length then {
      val boardData = (row until row + 5)
        .map(data(_))
        .map(_.split(" ").filterNot(_.isBlank).map(_.toInt).toIndexedSeq)
      Board(boardData) :: parseBoards(data, row + 6)
    } else List.empty
  }
}
