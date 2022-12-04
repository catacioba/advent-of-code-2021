package edu.aoc

import scala.collection.mutable

object Ch05 extends AocSolver {

  override def challenge(): String = "ch05"

  case class Point(x: Int, y: Int)

  case class Line(a: Point, b: Point) {
    def vhPoints(): Seq[Point] = {
      if (a.x == b.x) {
        val lowY = a.y.min(b.y)
        val highY = a.y.max(b.y)

        (lowY to highY).map(y => Point(a.x, y))
      } else if (a.y == b.y) {
        val lowX = a.x.min(b.x)
        val highX = a.x.max(b.x)

        (lowX to highX).map(x => Point(x, a.y))
      } else {
        Seq.empty
      }
    }

    def vhdPoints(): Seq[Point] = {
      if ((a.x - b.x).abs == (a.y - b.y).abs) {
        val deltaX = a.x - b.x
        val deltaY = a.y - b.y
        val delta = deltaX.abs
        val sgnX = if deltaX > 0 then 1 else -1
        val sgnY = if deltaY > 0 then 1 else -1

        (0 to delta).map(
          d => Point(a.x - sgnX * d, a.y - sgnY * d)
          )
      } else vhPoints()
    }
  }

  override def solvePart1(input: Seq[String]): Unit = {
    val lines = parseInput(input)
    val map = mutable.HashMap.empty[Point, Int]

    for (l <- lines;
         p <- l.vhPoints()
         ) {
      map.put(p, map.getOrElse(p, 0) + 1)
    }

    val cnt = map.values.count(_ > 1)

    println(cnt)
  }

  override def solvePart2(input: Seq[String]): Unit = {
    val lines = parseInput(input)
    val map = mutable.HashMap.empty[Point, Int]

    for (l <- lines;
         p <- l.vhdPoints()
         ) {
      map.put(p, map.getOrElse(p, 0) + 1)
    }

    val cnt = map.values.count(_ > 1)

    println(cnt)
  }

  private def parseInput(input: Seq[String]): List[Line] = {
    input.map(l => {
      val tokens = l.split("->")
      Line(parsePoint(tokens(0)), parsePoint(tokens(1)))
    }).toList
  }

  private def parsePoint(s: String): Point = {
    val tokens = s.trim.split(",")
    Point(
      tokens(0).toInt,
      tokens(1).toInt
      )
  }
}
