package edu.aoc

import scala.annotation.tailrec

object Ch17 extends AocSolver {

  override def challenge(): String = "ch17"

  case class TargetArea(x: Range, y: Range)

  private def simulateTrajectory(x: Int, y: Int): LazyList[(Int, Int)] = {
    def simulateTrajectoryAux(x: Int, y: Int, xVelocity: Int, yVelocity: Int): LazyList[(Int, Int)] = {
      val newX = x + xVelocity
      val newY = y + yVelocity
      (x, y) #:: simulateTrajectoryAux(newX, newY, adjustXVelocity(xVelocity), adjustYVelocity(yVelocity))
    }

    simulateTrajectoryAux(0, 0, x, y)
  }

  private def adjustXVelocity(x: Int): Int = {
    if x > 0 then x - 1 else if x < 0 then x + 1 else 0
  }

  private def adjustYVelocity(y: Int): Int = y - 1

  @tailrec
  private def hitsInTheZone(trajectory: LazyList[(Int, Int)], targetArea: TargetArea): Boolean = {
    val (x, y) = trajectory.head
    if targetArea.x.contains(x) && targetArea.y.contains(y) then true else {
      if (targetArea.x.max < x && x >= 0) || (targetArea.x.min > x && x < 0) then false else if targetArea
        .y
        .min > y then false else hitsInTheZone(trajectory.drop(1),
                                               targetArea)
    }
  }

  private def maxHeight(trajectory: LazyList[(Int, Int)]): Int = {
    if trajectory.head._2 > trajectory(1)._2 then trajectory.head._2 else
      math.max(trajectory.head._2, maxHeight(trajectory.drop(1)))
  }

  override def solvePart1(input: Seq[String]): Unit = {
    val targetArea = parseInput(input)

    val mh = (-200 to 200)
      .flatMap(x => (-200 to 200).map(y => simulateTrajectory(x, y)))
      .filter(t => hitsInTheZone(t, targetArea))
      .map(t => maxHeight(t))
      .max

    println(mh)
  }

  override def solvePart2(input: Seq[String]): Unit = {
    val targetArea = parseInput(input)

    val cnt = (-400 to 400)
      .flatMap(x => (-400 to 400).map(y => simulateTrajectory(x, y)))
      .count(t => hitsInTheZone(t, targetArea))

    println(cnt)
  }

  private def parseInput(input: Seq[String]): TargetArea = {
    val parts = input.head.stripPrefix("target area: ").split(", ")

    val xBounds = parts(0).stripPrefix("x=").split("\\.\\.").map(_.toInt)
    val yBounds = parts(1).stripPrefix("y=").split("\\.\\.").map(_.toInt)

    TargetArea(xBounds(0) to xBounds(1), yBounds(0) to yBounds(1))
  }
}
