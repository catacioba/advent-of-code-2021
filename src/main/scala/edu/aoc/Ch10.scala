package edu.aoc

import scala.annotation.tailrec

object Ch10 extends AocSolver {

  override def challenge(): String = "ch10"

  private val brackets: Map[Char, Char] = Map(
    '(' -> ')',
    '[' -> ']',
    '{' -> '}',
    '<' -> '>')
  private val inverseBrackets: Map[Char, Char] = brackets.map { case (a, b) => (b, a) }
  private val corruptedScores: Map[Char, Int] = Map(
    ')' -> 3,
    ']' -> 57,
    '}' -> 1197,
    '>' -> 25137)
  private val incompleteScores: Map[Char, Int] = Map(
    ')' -> 1,
    ']' -> 2,
    '}' -> 3,
    '>' -> 4)

  enum LineError:
    case Corrupted(score: Int)
    case Incomplete(score: Long)

  private def isOpening(c: Char): Boolean = brackets.contains(c)

  private def isClosing(c: Char): Boolean = inverseBrackets.contains(c)

  override def solvePart1(input: Seq[String]): Unit = {
    val score = input.flatMap(parseLine).map {
      case LineError.Corrupted(score) => score
      case _ => 0
    }.sum
    println(score)
  }

  override def solvePart2(input: Seq[String]): Unit = {
    val incompleteLines = input.flatMap(parseLine).flatMap {
      case LineError.Incomplete(score) => Some(score)
      case _ => None
    }.sorted

    val middleScore = incompleteLines(incompleteLines.length / 2)
    println(middleScore)
  }

  private def parseLine(s: String): Option[LineError] = {
    @tailrec
    def computeIncompleteScore(s: List[Char], score: Long): Long = {
      s match {
        case Nil => score
        case x :: xs => computeIncompleteScore(xs, score * 5 + incompleteScores(brackets(x)))
      }
    }

    @tailrec
    def parseLineAux(s: String, stack: List[Char]): Option[LineError] = {
      s match {
        case "" =>
          stack match {
            case st if st.nonEmpty => Some(LineError.Incomplete(computeIncompleteScore(st, 0)))
            case _ => None
          }
        case _ =>
          val c = s.head
          if isOpening(c) then {
            parseLineAux(s.tail, c :: stack)
          } else {
            stack.headOption match {
              case Some(h) if inverseBrackets(c) == h =>
                parseLineAux(s.tail, stack.tail)
              case _ => Some(LineError.Corrupted(corruptedScores(c)))
            }
          }
      }
    }

    parseLineAux(s, List.empty)
  }
}
