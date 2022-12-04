package edu.aoc

import scala.collection.mutable

object Ch19 extends AocSolver {

  override def challenge(): String = "ch19"

  case class Rotation(x: Int, y: Int, z: Int) {
    def inverse: Rotation = Rotation(4 - x, 4 - y, 4 - z)
  }

  object Rotation {
    lazy val allRotations: Seq[Rotation] = {

      val rotations = for (
        x <- 0 until 4;
        y <- 0 until 4;
        z <- 0 until 4
      ) yield Rotation(x, y, z)

      val p = Position(1, 2, 3)
      val groupedRotations = rotations.groupBy(r => p(r))
      groupedRotations.map {
        case (k, v) => v.head
      }.toSeq
    }

    lazy val rotationTransform: Map[Rotation, Rotation] = {
      val p = Position(1, 2, 3)

      (for (
        x <- allRotations;
        y <- allRotations if p == p(x)(y)
      ) yield x -> y).toMap
    }
  }

  case class Position(x: Int, y: Int, z: Int) {
    // rotate clockwise 90 degrees around the X axis.
    def rotateX: Position = {
      Position(x, -z, y)
    }

    // rotate clockwise 90 degrees around the X axis.
    def rotateY: Position = {
      Position(z, y, -x)
    }

    // rotate clockwise 90 degrees around the Z axis.
    def rotateZ: Position = {
      Position(-y, x, z)
    }

    def rotateX(c: Int): Position = {
      if c <= 0 then this else rotateX(c - 1).rotateX
    }

    def rotateY(c: Int): Position = {
      if c <= 0 then this else rotateY(c - 1).rotateY
    }

    def rotateZ(c: Int): Position = {
      if c <= 0 then this else rotateZ(c - 1).rotateZ
    }

    def apply(r: Rotation): Position = this.rotateX(r.x).rotateY(r.y).rotateZ(r.z)

    def +(that: Position): Position = {
      Position(x + that.x, y + that.y, z + that.z)
    }

    def -(that: Position): Position = {
      Position(x - that.x, y - that.y, z - that.z)
    }
  }

  override def solvePart1(input: Seq[String]): Unit = {
    val scanners = parseInput(input)

    //    val s0 = scanners(0)
    //    val s1 = scanners(1)
    //    val s2 = scanners(2)
    //    val s3 = scanners(3)
    //    val s4 = scanners(4)
    //
    //    println(findOffset(s0, s1))
    //    println(findOffset(s1, s4))
    val x = (for (
      a <- scanners.keys;
      b <- scanners.keys if a != b
    ) yield (a, b) -> findOffset(scanners(a), scanners(b))).flatMap {
      case (key, value) => value match {
        case Some(value) => Some(key -> value)
        case None => None
      }
    }.toMap

    //    val offset01 = findOffset(s0, s1)
    //    val offset14 = findOffset(s1, s4)
    println(x)
  }

  //  private def transformOffset(offset: Position, r: Rotation): Position = {
  //
  //  }

  private def findOffset(s1: Seq[Position], s2: Seq[Position]): Option[(Position, Rotation)] = {
    def checkOverlap(left: Seq[Position], right: Seq[Position]): Option[Position] = {
      val leftSet = left.toSet

      val offsets = for (
        x <- left;
        y <- right
      ) yield x - y

      val possibleOffsets = offsets
        //        .filter(p => p.x.abs <= 1000 && p.y.abs <= 1000 && p.z.abs <= 1000)
        .filter(o => right.map(r => r + o).count(leftSet.contains) >= 12)

      possibleOffsets.headOption
    }

    def checkRotation(r: Rotation): Option[(Position, Rotation)] = {
      val mappedPositions = s2.map(p => p(r))

      checkOverlap(s1, mappedPositions) match {
        case Some(offset) => Some((offset, r))
        case None => None
      }
    }

    val possibilities = Rotation.allRotations.flatMap(checkRotation)

    possibilities.headOption
  }

  override def solvePart2(input: Seq[String]): Unit = {}

  private def parseInput(input: Seq[String]): Map[Int, Seq[Position]] = {
    def parseScanner(lines: Seq[String]): List[(Int, Seq[Position])] = {
      if lines.isEmpty then Nil else {
        val s = lines.head.stripPrefix("--- scanner ").stripSuffix(" ---").toInt
        val left = lines.drop(1)
        val idx = left.indexWhere(l => l.isBlank)
        val positions = (if idx >= 0 then left.take(idx) else left).map(l => {
          val parts = l.split(",")
          Position(parts(0).toInt, parts(1).toInt, parts(2).toInt)
        })
        (s -> positions) :: parseScanner(if idx >= 0 then left.drop(idx + 1) else Seq.empty)
      }
    }

    parseScanner(input).toMap
  }
}
