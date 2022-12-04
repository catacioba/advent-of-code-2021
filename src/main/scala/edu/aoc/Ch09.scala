package edu.aoc

import scala.collection.mutable

object Ch09 extends AocSolver {
  override def challenge(): String = "ch09"

  case class Board(map: Array[Array[Int]]) {

    enum Direction(val dx: Int, val dy: Int):
      case Up extends Direction(-1, 0)
      case Down extends Direction(1, 0)
      case Left extends Direction(0, -1)
      case Right extends Direction(0, 1)

    def height: Int = map.length

    def width: Int = map.head.length

    def lowPoints(): Seq[(Int, Int)] = {
      (for (
        x <- 0 until height;
        y <- 0 until width
      ) yield {
        val lowest = neighbors(x, y).forall(t => t match {
          case (a, b) => map(a)(b) > map(x)(y)
        })
        if lowest then Some((x, y)) else None
      }).flatten
    }

    def flood(x: Int, y: Int): Int = {
      val visited = mutable.Set.empty[(Int, Int)]

      def floodAux(x: Int, y: Int): Int = {
        if (visited.contains((x, y))) {
          return 0
        }
        visited.add((x, y))

        1 + neighbors(x, y).filterNot {
          case (a, b) =>
            map(a)(b) == 9
        }.filterNot(visited.contains).map {
          case (a, b) =>
            floodAux(a, b)
        }.sum
      }

      floodAux(x, y)
    }

    def neighbors(x: Int, y: Int): Seq[(Int, Int)] = {
      Direction.values.map(d => (x + d.dx, y + d.dy)).filter {
        case (x, y) => isValid(x, y)
      }
    }

    def isValid(x: Int, y: Int): Boolean = {
      x >= 0 && x < height && y >= 0 && y < width
    }
  }

  override def solvePart1(input: Seq[String]): Unit = {
    val board = parseInput(input)

    val riskLevel = board.lowPoints().map {
      case (x, y) => 1 + board.map(x)(y)
    }.sum
    println(riskLevel)
  }

  override def solvePart2(input: Seq[String]): Unit = {
    val board = parseInput(input)

    val basins = board.lowPoints().map {
      case (x, y) => board.flood(x, y)
    }.sorted(Ordering.Int.reverse)

    println(basins)
    println(basins.take(3).product)
  }

  private def parseInput(input: Seq[String]): Board = {
    Board(input.map(s => s.toCharArray.map(_.asDigit)).toArray)
  }
}
