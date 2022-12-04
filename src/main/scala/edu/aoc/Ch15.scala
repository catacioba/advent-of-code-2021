package edu.aoc

import scala.collection.mutable
import scala.math.sqrt

object Ch15 extends AocSolver {

  override def challenge(): String = "ch15"

  case class Board(map: IndexedSeq[IndexedSeq[Int]]) {

    private val INF: Int = Int.MaxValue - 10000

    def height: Int = map.length

    def width: Int = map.head.length

    def dijkstra(): Option[Int] = {
      val start = (0, 0)
      val end = (height - 1, width - 1)

      val dist = mutable.Map.empty[(Int, Int), Int]

      (start._1 to end._1).foreach(x => (start._2 to end._2).foreach(y => dist.update((x, y), INF)))

      val queue = mutable.PriorityQueue.empty[Step](Ordering.by(x => -x.cost))
      queue.enqueue(Step(start, 0))

      while queue.nonEmpty do {
        val step = queue.dequeue()
        val node = step.position
        val cost = step.cost

        if node == end then return Some(cost)

        neighbors(node).foreach(n => {
          val newCost = cost + map(n._1)(n._2)
          if newCost < dist(n) then {
            dist.update(n, newCost)
            queue.enqueue(Step(n, newCost))
          }
        })
      }

      None
    }

    def aStar(): Option[Int] = {
      val start = (0, 0)
      val end = (height - 1, width - 1)

      val dist = mutable.Map.empty[(Int, Int), Int]

      def h(x: Int, y: Int): Int = sqrt(x * x + y * y).toInt

      val queue = mutable.PriorityQueue.empty[Step](Ordering.by(x => -(x.cost + h(x.position._1, x.position._2))))
      queue.enqueue(Step(start, 0))

      while queue.nonEmpty do {
        val step = queue.dequeue()
        val node = step.position
        val cost = step.cost

        if node == end then return Some(cost)

        neighbors(node).foreach(n => {
          val newCost = cost + map(n._1)(n._2)
          if !dist.contains(n) || newCost < dist(n) then {
            dist.update(n, newCost)
            queue.enqueue(Step(n, newCost))
          }
        })
      }

      None
    }

    case class Step(position: (Int, Int), cost: Int)

    enum Direction(val dx: Int, val dy: Int):
      case Up extends Direction(-1, 0)
      case Down extends Direction(1, 0)
      case Left extends Direction(0, -1)
      case Right extends Direction(0, 1)

    def neighbors(x: Int, y: Int): Seq[(Int, Int)] = {
      Direction.values.map(d => (x + d.dx, y + d.dy)).filter {
        case (x, y) => isValid(x, y)
      }
    }

    def neighbors(p: (Int, Int)): Seq[(Int, Int)] = neighbors(p._1, p._2)

    def isValid(x: Int, y: Int): Boolean = {
      x >= 0 && x < height && y >= 0 && y < width
    }
  }

  override def solvePart1(input: Seq[String]): Unit = {
    val board = Board(parseInput(input))

    println(board.dijkstra())
  }

  override def solvePart2(input: Seq[String]): Unit = {
    val initialMap = parseInput(input)

    val largerMap = (0 until 5).flatMap(x => initialMap.map(l => (0 until 5).flatMap(y => l.map(z => {
      val newValue = z + y + x
      if newValue > 9 then newValue - 9 else newValue
    }))))

    val board = Board(largerMap)
    println(board.aStar())
  }

  private def parseInput(input: Seq[String]): IndexedSeq[IndexedSeq[Int]] = {
    input.map(l => l.toCharArray.map(_.asDigit).toIndexedSeq).toIndexedSeq
  }
}
