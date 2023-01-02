package edu.aoc

import scala.collection.mutable

object Ch23 extends AocSolver {

  override def challenge(): String = "ch23"

  class Board(val map: Array[Array[Char]], roomSize: Int) {

    enum DragonType:
      case A, B, C, D

    object DragonType {
      def fromChar(c: Char): DragonType = c match
        case 'A' => DragonType.A
        case 'B' => DragonType.B
        case 'C' => DragonType.C
        case 'D' => DragonType.D
    }

    private val cost = Map(
      DragonType.A -> 1,
      DragonType.B -> 10,
      DragonType.C -> 100,
      DragonType.D -> 1000
    )

    private val dragonYs = Map(
      DragonType.A -> 3,
      DragonType.B -> 5,
      DragonType.C -> 7,
      DragonType.D -> 9
    )

    private val dragonChars = List('A', 'B', 'C', 'D')

    case class Position(x: Int, y: Int) {
      def +(ot: Position): Position = Position(x + ot.x, y + ot.y)
    }

    case class Dragon(id: Int, p: Position, t: DragonType) {
      def isInHallway: Boolean = p.x == 1

      def isInRoom(dragonType: DragonType): Boolean =
        p.x > 1 && p.y == dragonYs(dragonType)

      def isInRightRoom: Boolean = isInRoom(t)

      def isRightOutsideRoom: Boolean =
        p.x == 1 && dragonYs.values.exists(_ == p.y)
    }

    case class State(dragons: List[Dragon], energy: Int) {
      def isSolved: Boolean =
        dragons.groupBy(_.t).forall { case (dragonType, dragonsByType) =>
          dragonsByType.forall(d =>
            d.p.y == dragonYs(dragonType)
          ) && dragonsByType.map(_.p.x).sorted == (2 to (roomSize + 1)).toList
        }

      private def areInRightRoom(dragon: Dragon): Boolean = {
        val dragonsInRoom =
          dragons.filter(d => d.p.x > 1 && d.p.y == dragon.p.y)
        if (dragonsInRoom.size > roomSize) {
          throw IllegalArgumentException("invalid number of dragons in room")
        }
        dragon.isInRightRoom &&
        dragonsInRoom.forall(d => d.t == dragon.t) &&
        (dragonsInRoom
          .map(_.p.x)
          .sorted == ((roomSize + 2 - dragonsInRoom.size) to (roomSize + 1)).toVector)
      }

      def getPossibleMoves: List[State] = {
        val hallwayMoves = dragons
          .filterNot(_.isInHallway)
          .filterNot(areInRightRoom)
          .flatMap(reachablePositions)
          .filter { case (dragon, _) =>
            dragon.isInHallway
          }
          .map { case (dragon, dist) =>
            State(
              replaceDragon(dragons, dragon),
              energy + dist * cost(dragon.t)
            )
          }
        val roomMoves = dragons
          .filter(_.isInHallway)
          .flatMap(reachablePositions)
          .filter { case (dragon, _) =>
            areInRightRoom(dragon)
          }
          .map { case (dragon, dist) =>
            State(
              replaceDragon(dragons, dragon),
              energy + dist * cost(dragon.t)
            )
          }

        hallwayMoves ++ roomMoves
      }

      private def replaceDragon(
          dragons: List[Dragon],
          dragon: Dragon
      ): List[Dragon] = {
        dragons.map(d => if d.id == dragon.id then dragon else d)
      }

      private val deltas =
        List(Position(1, 0), Position(0, 1), Position(-1, 0), Position(0, -1))

      private def reachablePositions(
          start: Dragon
      ): mutable.Map[Dragon, Int] = {
        val queue = mutable.ArrayDeque[Dragon]()
        val dist = mutable.Map[Dragon, Int]()

        queue.addOne(start)
        dist.put(start, 0)

        while (queue.nonEmpty) {
          val dragon = queue.removeHead()
          val d = dist(dragon)

          deltas
            .map(delta => Dragon(dragon.id, delta + dragon.p, dragon.t))
            .filter(dragon =>
              map(dragon.p.x)(dragon.p.y) != '#' &&
                !dragons.exists(d => d.p == dragon.p)
            )
            .filterNot(p => dist.contains(p))
            .foreach { dragon =>
              { queue.addOne(dragon); dist.put(dragon, d + 1) }
            }
        }

        dist.remove(start)
        dist.filterNot { case (dragon, _) =>
          dragon.isRightOutsideRoom
        }
      }
    }

    private val startState = State(getDragons, 0)

    def leastEnergy(): Int = {
      val dist = mutable.Map.empty[List[Dragon], Int]
      val priorityQueue =
        mutable.PriorityQueue.empty[State]((s1, s2) => s2.energy - s1.energy)
      priorityQueue.addOne(startState)

      while (priorityQueue.nonEmpty) {
        val state = priorityQueue.dequeue()

        if (state.isSolved) {
          return state.energy
        }

        state.getPossibleMoves
          .foreach(ns => {
            if (!dist.contains(ns.dragons) || dist(ns.dragons) > ns.energy) {
              dist.put(ns.dragons, ns.energy)
              priorityQueue.addOne(ns)
            }
          })
      }

      Int.MaxValue
    }

    private def getDragons: List[Dragon] = {
      var id = 0
      map.indices
        .flatMap(x =>
          map(x).indices
            .filter(y => dragonChars.contains(map(x)(y)))
            .map(y => {
              id += 1
              Dragon(id, Position(x, y), DragonType.fromChar(map(x)(y)))
            })
        )
        .toList
    }
  }

  object Board {
    def fromLines(lines: Seq[String], roomSize: Int): Board = {
      Board(lines.map(l => l.toCharArray).toArray, roomSize)
    }
  }

  override def solvePart1(input: Seq[String]): Unit = {
    val board = Board.fromLines(input, 2)

    println(board.leastEnergy())
  }

  override def solvePart2(input: Seq[String]): Unit = {
    val newInput =
      input.slice(0, 3) ++ List("  #D#C#B#A#", "  #D#B#A#C#") ++ input.slice(
        3,
        input.size
      )
    val board = Board.fromLines(newInput, 4)

    println(board.leastEnergy())
  }
}
