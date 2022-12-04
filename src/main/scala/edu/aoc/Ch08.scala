package edu.aoc

import scala.collection.mutable

object Ch08 extends AocSolver {

  override def challenge(): String = "ch08"

  case class Line(left: Seq[String], right: Seq[String]) {
    def tokens(): Seq[String] = left ++ right
  }

  private val byCount = Map(
    2 -> Seq(1),
    3 -> Seq(7),
    4 -> Seq(4),
    5 -> Seq(2, 3, 5),
    6 -> Seq(0, 6, 9),
    7 -> Seq(8))

  private val valuesByInt = Map(
    0 -> Set('a', 'b', 'c', 'e', 'f', 'g'),
    1 -> Set('c', 'f'),
    2 -> Set('a', 'c', 'd', 'e', 'g'),
    3 -> Set('a', 'c', 'd', 'f', 'g'),
    4 -> Set('b', 'c', 'd', 'f'),
    5 -> Set('a', 'b', 'd', 'f', 'g'),
    6 -> Set('a', 'b', 'd', 'e', 'f', 'g'),
    7 -> Set('a', 'c', 'f'),
    8 -> Set('a', 'b', 'c', 'd', 'e', 'f', 'g'),
    9 -> Set('a', 'b', 'c', 'd', 'f', 'g'))

  private val values: Set[String] = valuesByInt.values.map(x => x.mkString.sorted).toSet
  private val valuesByChars: Map[String, Int] = valuesByInt.map(x => (x._2.mkString.sorted, x._1))
  private val unique: Set[Int] = byCount.filter(x => x._2.length == 1).keys.toSet

  override def solvePart1(input: Seq[String]): Unit = {
    val data = parseInput(input)

    val cnt = data.flatMap(l => l.right.map(t => t.length match {
      case 2 | 3 | 4 | 7 => 1
      case _ => 0
    })).sum

    println(cnt)
  }

  override def solvePart2(input: Seq[String]): Unit = {
    val data = parseInput(input)

    val sum = data.map(solveLine).sum
    println(sum)
  }

  //  private def solveSmart(line: Line): Int = {
  //    val mapping = mutable.Map.empty[Char, Char]
  //    val tokenMapping = mutable.Map.empty[Set[Char], Set[Char]]
  //
  //    line.tokens().filter(t => unique.contains(t.length)).foreach(
  //      t => {
  //        val mapping = t.toCharArray.toSet
  //        val correctMapping = values(byCount(t.length).head)
  //
  //        tokenMapping.put(mapping, correctMapping)
  //      })
  //
  //    for (
  //      a <- tokenMapping;
  //      b <- tokenMapping
  //    ) {
  //      val (fromA, toA) = a
  //      val (fromB, toB) = b
  //
  //      val fromIntersect = fromA.intersect(fromB)
  //      val toIntersect = toA.intersect(toB)
  //      tokenMapping.put(fromIntersect, toIntersect)
  //
  //      fromA.contains
  //    }
  //
  //    println(tokenMapping.filter(x => x._1.size == 1))
  //  }

  private def solveLine(line: Line): Int = {
    val perms = possibilities()
    val perm = perms.filter(p => checkPermutation(line, p)).head
    line.right.map(t => valuesByChars(t.map(perm).sorted)).mkString.toInt
  }

  private def checkPermutation(line: Line, mapping: Map[Char, Char]): Boolean = {
    line.tokens().forall(t => values.contains(t.map(mapping).sorted))
  }

  private def possibilities(): Seq[Map[Char, Char]] = {
    val chars = Seq('a', 'b', 'c', 'd', 'e', 'f', 'g')
    chars.permutations.map(p => chars.zip(p).toMap).toSeq
  }

  private def parseInput(input: Seq[String]): Seq[Line] = {
    input.map(l => {
      val parts = l.split("\\|")
      val left = parseSide(parts(0))
      val right = parseSide(parts(1))
      Line(left, right)
    })
  }

  private def parseSide(side: String): Seq[String] = {
    side.split(" ").filterNot(_.isBlank)
  }
}
