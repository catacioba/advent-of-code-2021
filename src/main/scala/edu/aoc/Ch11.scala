package edu.aoc

import scala.collection.mutable

object Ch11 extends AocSolver {

  override def challenge(): String = "ch11"

  class Board(val map: Array[Array[Int]]) {

    var flashCount: Int = 0

    enum Direction(val dx: Int, val dy: Int):
      case Up extends Direction(-1, 0)
      case Down extends Direction(1, 0)
      case Left extends Direction(0, -1)
      case Right extends Direction(0, 1)
      case UpLeft extends Direction(-1, -1)
      case UpRight extends Direction(-1, 1)
      case DownLeft extends Direction(1, -1)
      case DownRight extends Direction(1, 1)

    def height: Int = map.length

    def width: Int = map.head.length

    def iterate(): Unit = {
      val visited = mutable.Set.empty[(Int, Int)]
      iterateMap((x, y) => addOne(x, y, visited))
      iterateMap(clearFlashed)
    }

    private def addOne(x: Int, y: Int, visited: mutable.Set[(Int, Int)]): Unit = {
      val newValue = map(x)(y) + 1
      map(x).update(y, newValue)
      if newValue > 9 && !visited.contains((x, y)) then flash(x, y, visited)
    }

    private def flash(x: Int, y: Int, visited: mutable.Set[(Int, Int)]): Unit = {
      visited.add((x, y))
      neighbors(x, y).foreach((x, y) => addOne(x, y, visited))
    }

    def clearFlashed(x: Int, y: Int): Unit = {
      if map(x)(y) > 9 then {
        flashCount += 1
        map(x).update(y, 0)
      }
    }

    def findFirstSync(): Int = {
      (1 until Int.MaxValue).find(_ => {
        iterate()
        map.forall(l => l.forall(_ == 0))
      }).get
    }

    private def iterateMap(f: (Int, Int) => Unit): Unit = {
      (0 until height).foreach(x => (0 until width).foreach(y => f(x, y)))
    }

    def neighbors(x: Int, y: Int): Seq[(Int, Int)] = {
      Direction.values.map(d => (x + d.dx, y + d.dy)).filter {
        case (x, y) => isValid(x, y)
      }
    }

    def isValid(x: Int, y: Int): Boolean = {
      x >= 0 && x < height && y >= 0 && y < width
    }

    def print(): Unit = {
      map.map(l => l.mkString).foreach(println)
      println()
    }
  }

  override def solvePart1(input: Seq[String]): Unit = {
    val board = parseInput(input)

    (0 until 100).foreach(x => {
      board.iterate()
    })

    println(board.flashCount)
  }

  override def solvePart2(input: Seq[String]): Unit = {
    val board = parseInput(input)

    println(board.findFirstSync())
  }

  def parseInput(input: Seq[String]): Board = {
    Board(input.map(l => l.toCharArray.map(_.asDigit)).toArray)
  }
}
