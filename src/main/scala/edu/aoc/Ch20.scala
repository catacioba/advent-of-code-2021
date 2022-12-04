package edu.aoc

import scala.collection.mutable

object Ch20 extends AocSolver {

  override def challenge(): String = "ch20"

  enum Pixel:
    case Light
    case Dark

    def toBinary: Char = this match {
      case Light => '1'
      case Dark => '0'
    }

  end Pixel

  object Pixel {
    def fromChar(c: Char): Pixel = {
      c match {
        case '.' => Pixel.Dark
        case '#' => Pixel.Light
      }
    }
  }

  case class ImageEnhancementAlgorithm(line: IndexedSeq[Pixel]) {
  }

  case class Image(pixels: mutable.Map[(Int, Int), Pixel]) {
    def enhance(algo: ImageEnhancementAlgorithm): Image = {
      //      val affectedPositions = lightPixels.flatMap(neighbors)
      //      val minX = lightPixels.minBy(_._1)._1
      //      val maxX = lightPixels.maxBy(_._1)._1
      //      val minY = lightPixels.minBy(_._2)._2
      //      val maxY = lightPixels.maxBy(_._2)._2
      //
      //      val affectedPositions = mutable
      //        .Set
      //        .from(minX - 1 to maxX + 1)
      //        .flatMap(x => (minY - 1 to maxY + 1).map(y => (x, y)))
      //
      //      val newLightPixels = affectedPositions.flatMap(p => enhancedPixel(p, algo))
      //
      //      Image(newLightPixels)

      //      val positions = pixels.keySet
      //
      //      val minX = positions.minBy(_._1)._1
      //      val maxX = positions.maxBy(_._1)._1
      //      val minY = positions.minBy(_._2)._2
      //      val maxY = positions.maxBy(_._2)._2

      val positions = pixels.keySet.flatMap(neighbors)

      //      val newPixels = pixels.map {
      val newPixels = mutable.Map.empty[(Int, Int), Pixel]

      positions.foreach {
        case (x, y) =>
          //          val (x, y) = pos
          val bs = (-1 to 1)
            .flatMap(dx => (-1 to 1).map(dy => pixels.getOrElse((x + dx, y + dy), Pixel.Dark).toBinary))
            .mkString
          val p = Integer.parseInt(bs, 2)
          val newPixel = algo.line(p)

          //          (x, y) -> newPixel
          newPixels.put((x, y), newPixel)
      }

      Image(newPixels)
    }

    private def neighbors(p: (Int, Int)): Seq[(Int, Int)] = p match {
      case (x, y) => (-1 to 1).flatMap(dx => (-1 to 1).map(dy => (x + dx, y + dy)))
    }

    //    private def enhancedPixel(p: (Int, Int), algo: ImageEnhancementAlgorithm): Option[(Int, Int)] = {
    //      val bs = p match {
    //        case (x, y) => (-1 to 1)
    //          .flatMap(dx => (-1 to 1).map(dy => (x + dx, y + dy)))
    //          .map(p => if lightPixels.contains(p) then Pixel.Light else Pixel.Dark)
    //          .map(_.toBinary)
    //
    //      }
    //      val pos = Integer.parseInt(bs.mkString, 2)
    //      algo.line(pos) match {
    //        case Pixel.Light => Some(p)
    //        case Pixel.Dark => None
    //      }
    //    }

    def litPixels: Int = pixels.count {
      case (_, p) => p match {
        case Pixel.Light => true
        case Pixel.Dark => false
      }
    }
  }

  override def solvePart1(input: Seq[String]): Unit = {
    val (algo, img) = parseInput(input)

    //    val litPixels = img.enhance(algo).enhance(algo).lightPixels.size
    val litPixels = img.enhance(algo).enhance(algo).litPixels

    println(litPixels)
  }

  override def solvePart2(input: Seq[String]): Unit = {}

  private def parseInput(input: Seq[String]): (ImageEnhancementAlgorithm, Image) = {
    val algo = ImageEnhancementAlgorithm(input.head.map(Pixel.fromChar))

    val map = input.drop(2).map(l => l.map(Pixel.fromChar)).toIndexedSeq

    //    val lightPixels = mutable.Set.empty[(Int, Int)]
    //
    //    map.indices.map(x => map(x).indices.map(y => lightPixels.add((x, y))))
    //
    //    val img = Image(lightPixels)

    val pixels = mutable.Map.empty[(Int, Int), Pixel]

    map.indices.map(x => map(x).indices.map(y => pixels.put((x, y), map(x)(y))))

    val img = Image(pixels)

    (algo, img)
  }
}
