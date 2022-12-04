package edu.aoc

import scala.io.{BufferedSource, Source}

object Ch02 extends AocSolver {

  override def challenge(): String = "ch02"

  enum Direction {
    case forward, up, down
  }

  case class Move(direction: Direction, value: Int)

  case class State(horizontal: Int, depth: Int) {
    def move(m: Move): State = {
      m.direction match {
        case Direction.forward => State(horizontal + m.value, depth)
        case Direction.down => State(horizontal, depth + m.value)
        case Direction.up => State(horizontal, depth - m.value)
      }
    }
  }

  override def solvePart1(input: Seq[String]): Unit = {

    val state = parseInput(input)
      .foldLeft(State(0, 0))((state, move) => state.move(move))

    println(state.depth * state.horizontal)
  }

  case class AimState(horizontal: Int, depth: Int, aim: Int) {
    def move(m: Move): AimState = {
      m.direction match {
        case Direction.forward => AimState(horizontal + m.value, depth + aim * m.value, aim)
        case Direction.down => AimState(horizontal, depth, aim + m.value)
        case Direction.up => AimState(horizontal, depth, aim - m.value)
      }
    }
  }

  override def solvePart2(input: Seq[String]): Unit = {
    val state = parseInput(input)
      .foldLeft(AimState(0, 0, 0))((state, move) => state.move(move))

    println(state.depth * state.horizontal)
  }

  private def parseInput(input: Seq[String]): Seq[Move] = {
    input.map(line => {
      val tokens = line.split(' ')
      Move(Direction.valueOf(tokens(0)), tokens(1).toInt)
    })
  }
}
