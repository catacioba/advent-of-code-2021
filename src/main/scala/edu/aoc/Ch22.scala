package edu.aoc

import scala.annotation.tailrec
import scala.collection.mutable

object Ch22 extends AocSolver {

  override def challenge(): String = "ch22"

  case class Cuboid(x: Int, y: Int, z: Int) {
    def isValid: Boolean = {
      x >= -50 && y >= -50 && z >= -50 && x <= 50 && y <= 50 && z <= 50
    }
  }

  case class CuboidRanges(x: Range, y: Range, z: Range, on: Boolean) {
    def cuboids: (Boolean, IndexedSeq[Cuboid]) = {
      def range(x: Range): Range = {
        math.max(x.start, -50) to math.min(x.end, 50)
      }

      val cuboids = for (
        xx <- range(x);
        yy <- range(y);
        zz <- range(z)
      ) yield Cuboid(xx, yy, zz)

      (on, cuboids)
    }
  }

  object CuboidRanges {
    def fromLine(l: String): CuboidRanges = {
      val tokens = l.split(" ")
      val on = tokens(0) == "on"
      val ranges = tokens(1).split(",")
                            .map(t => {
                              val ends = t.split("=")(1).split("\\.\\.").map(_.toInt)
                              ends(0) to ends(1)
                            })

      CuboidRanges(ranges(0), ranges(1), ranges(2), on)
    }
  }

  override def solvePart1(input: Seq[String]): Unit = {
    val cuboidRanges = input.map(CuboidRanges.fromLine)

    val onCuboids = mutable.Set.empty[Cuboid]

    checkRanges(cuboidRanges, onCuboids)

    println(onCuboids.size)
  }

  @tailrec
  private def checkRanges(ranges: Seq[CuboidRanges], onCuboids: mutable.Set[Cuboid]): Unit = {
    ranges match {
      case ::(range, next) =>
        val (on, cuboids) = range.cuboids

        if on then {
          cuboids.foreach(onCuboids.add)
        } else {
          cuboids.foreach(onCuboids.remove)
        }

        checkRanges(next, onCuboids)
      case Nil =>
    }
  }

  case class Interval(start: Int, end: Int) {
    //        def overlaps(other: Interval): Boolean = {
    //          start >= other.end &&
    //        }
  }

  case class CuboidIntervals(x: Interval, y: Interval, z: Interval)

  override def solvePart2(input: Seq[String]): Unit = {
    //    val s = mutable.SortedSet.empty((Interval, mutable.SortedSet.empty[(Interval, Seq[Interval])]))
    //
    //    val cuboidRanges = input.map(CuboidRanges.fromLine)


  }

}
