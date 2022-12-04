package edu.aoc

object Ch13 extends AocSolver {

  override def challenge(): String = "ch13"

  case class Dot(x: Int, y: Int) {
    def foldBy(fold: Fold): Option[Dot] = {
      fold match {
        case Fold.Vertical(foldX) =>
          if x == foldX then None else
            Some(Dot(if x >= foldX then x - 2 * (x - foldX) else x, y))
        case Fold.Horizontal(foldY) =>
          if y == foldY then None else
            Some(Dot(x, if y > foldY then y - 2 * (y - foldY) else y))
      }
    }
  }

  enum Fold:
    case Vertical(x: Int) extends Fold
    case Horizontal(y: Int) extends Fold

  override def solvePart1(input: Seq[String]): Unit = {
    val (dots, folds) = parseInput(input)
    val m = fold(dots, folds.head).size
    println(m)
  }

  private def fold(dots: Set[Dot], fold: Fold): Set[Dot] = {
    dots.flatMap(d => d.foldBy(fold))
  }

  def printBoard(dots: Set[Dot]): Unit = {
    val minX = dots.map(_.x).min
    val maxX = dots.map(_.x).max
    val minY = dots.map(_.y).min
    val maxY = dots.map(_.y).max

    (minY to maxY).foreach(y => {
      (minX to maxX).foreach(x => print(if dots.contains(Dot(x, y)) then '#' else '.'))
      println()
    })
    println()
  }

  override def solvePart2(input: Seq[String]): Unit = {
    val (dots, folds) = parseInput(input)
    val foldedBoard = folds.foldLeft(dots)((d, f) => fold(d, f))
    printBoard(foldedBoard)
  }

  private def parseInput(input: Seq[String]): (Set[Dot], Seq[Fold]) = {
    val sepIndex = input.indexWhere(x => x.isBlank)

    val (dots, folds) = input.splitAt(sepIndex)

    (
      dots.map(d => {
        val tokens = d.split(",")
        Dot(tokens(0).toInt, tokens(1).toInt)
      }).toSet,
      folds.drop(1).map(f => {
        val tokens = f.stripPrefix("fold along ").split("=")
        val n = tokens(1).toInt
        tokens(0) match {
          case "y" => Fold.Horizontal(n)
          case "x" => Fold.Vertical(n)
        }
      })
    )
  }
}
