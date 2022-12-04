package edu.aoc

import scala.annotation.tailrec
import scala.collection.mutable

object Ch21 extends AocSolver {

  override def challenge(): String = "ch21"

  case class Player(position: Int, score: Int) {
    def move(p: Int): Player = {
      val newPosition = modulo(position + p, 10)
      Player(newPosition, score + newPosition)
    }

    def moveDice(dice: Int): Player = {
      this.move(modulo(dice, 100) + modulo(dice + 1, 100) + modulo(dice + 2, 100))
    }

    lazy val dies: List[List[Int]] = {
      val values = List(1, 2, 3)
      for (x <- values;
           y <- values;
           z <- values) yield List(x, y, z)
    }

    lazy val quantumMoveCounts: Seq[(Int, Int)] = {
      dies.groupMapReduce(_.sum)(_ => 1)(_ + _).toSeq
    }

    //    def moveQuantumDice: List[Player] = {
    //      dies.map {
    //        case x :: y :: z :: Nil => this.move(x + y + z)
    //      }
    //    }

    //    def moveQuantumDice2: Unit = {
    //      dies.map
    //        case
    //    }
  }

  @tailrec
  private def modulo(v: Int, m: Int): Int = {
    if v > m then modulo(v - m, m) else v
  }

  override def solvePart1(input: Seq[String]): Unit = {
    val p1 = parsePlayer(input.head)
    val p2 = parsePlayer(input(1))

    val (winner, loser, rolls) = play(p1, p2, 1)

    println(loser.score * rolls)
  }

  private def play(p1: Player, p2: Player, dice: Int): (Player, Player, Int) = {
    @tailrec
    def playAux(p1: Player, p2: Player, dice: Int, firstPlayerPlays: Boolean, rolls: Int): (Player, Player, Int) = {
      //            println(s"$p1 $p2 $dice $firstPlayerPlays $rolls")
      if p1.score >= 1000 then return (p1, p2, rolls)
      if p2.score >= 1000 then return (p2, p1, rolls)

      if firstPlayerPlays then {
        playAux(p1.moveDice(dice),
                p2,
                modulo(dice + 3, 100), !firstPlayerPlays, rolls + 3)
      } else {
        playAux(p1,
                p2.moveDice(dice),
                modulo(dice + 3, 100), !firstPlayerPlays, rolls + 3)
      }
    }

    playAux(p1, p2, 1, true, 0)
  }

  override def solvePart2(input: Seq[String]): Unit = {
    val p1 = parsePlayer(input.head)
    val p2 = parsePlayer(input(1))

    val (p1Won, p2Won) = playQuantum(p1, p2)
    //    println(s"$p1Won - $p2Won")

    println(math.max(p1Won, p2Won))
  }

  case class GameState(p1: Player, p2: Player, firstPlayerPlays: Boolean)

  private def playQuantum(p1: Player, p2: Player): (Long, Long) = {
    var steps = 1
    val memoizedGameStates = mutable.Map.empty[GameState, (Long, Long)]

    def playQuantumAux(p1: Player, p2: Player, firstPlayerPlays: Boolean): (Long, Long) = {
      val gameState = GameState(p1, p2, firstPlayerPlays)
      steps += 1
      //      println(s"$gameState")

      if memoizedGameStates.contains(gameState) then memoizedGameStates(gameState)

      if p1.score >= 21 then return (1, 0)
      if p2.score >= 21 then return (0, 1)

      val x = if firstPlayerPlays then {
        p1
          .quantumMoveCounts
          .map {
            case (move, count) =>
              val (p1Wins, p2Wins) = playQuantumAux(p1.move(move), p2, !firstPlayerPlays)
              (count * p1Wins, count * p2Wins)
          }
      } else {
        p2
          .quantumMoveCounts
          .map {
            case (move, count) =>
              val (p1Wins, p2Wins) = playQuantumAux(p1, p2.move(move), !firstPlayerPlays)
              (count * p1Wins, count * p2Wins)
          }
      }

      val res = x.reduce((a, b) => {
        //        println("aa")
        (a._1 + b._1, a._2 + b._2)
      })

      memoizedGameStates.put(gameState, res)

      res
    }

    playQuantumAux(p1, p2, true)
  }

  //  private def playQuantumDp(p1: Player, p2: Player): (Long, Long) = {
  //
  //  }

  private def parsePlayer(s: String): Player = {
    val r = raw"Player (\d+) starting position: (\d+)".r
    val groups = r.findFirstMatchIn(s).get
    Player(groups.group(2).toInt, 0)
  }

}
