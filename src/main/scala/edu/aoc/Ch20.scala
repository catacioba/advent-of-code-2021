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

    def toChar: Char = this match {
      case Light => '#'
      case Dark => '.'
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
    def enhance(algo: ImageEnhancementAlgorithm, hiddenPixels: Pixel): Image = {
      val positions = pixels.keySet.flatMap(affectedPixels)

      val newPixels = mutable.Map.empty[(Int, Int), Pixel]

      positions.foreach {
        case (x, y) =>
          val bs = (-1 to 1)
            .flatMap(dx => (-1 to 1).map(dy => pixels.getOrElse((x + dx, y + dy), hiddenPixels).toBinary))
            .mkString
          val p = Integer.parseInt(bs, 2)
          val newPixel = algo.line(p)

          newPixels.put((x, y), newPixel)
      }

      Image(newPixels)
    }

    private def affectedPixels(p: (Int, Int)): Seq[(Int, Int)] = p match {
      case (x, y) => (-2 to 2).flatMap(dx => (-2 to 2).map(dy => (x + dx, y + dy)))
    }

    def litPixels: Int = pixels.count {
      case (_, p) => p match {
        case Pixel.Light => true
        case Pixel.Dark => false
      }
    }

    def draw(): Unit = {
      val minX = pixels.minBy(_._1._1)._1._1
      val maxX = pixels.maxBy(_._1._1)._1._1
      val minY = pixels.minBy(_._1._2)._1._2
      val maxY = pixels.maxBy(_._1._2)._1._2

      (minX to maxX).foreach(x => {
        (minY to maxY).foreach(y => {
          print(pixels.getOrElse((x, y), Pixel.Dark).toChar)
        })
        println()
      })
    }
  }

  private def getNewHiddenPixelValue(algorithm: ImageEnhancementAlgorithm, hidden: Pixel) =
    hidden match {
      case Pixel.Dark => algorithm.line.head
      case Pixel.Light => algorithm.line.last
    }

  override def solvePart1(input: Seq[String]): Unit = {
    val (algo, img) = parseInput(input)

    var hiddenPixels = Pixel.Dark
    var enhancedImg = img.enhance(algo, hiddenPixels)

    hiddenPixels = getNewHiddenPixelValue(algo, hiddenPixels)
    enhancedImg = enhancedImg.enhance(algo, hiddenPixels)

    val litPixels = enhancedImg.litPixels

    println(litPixels)
  }

  override def solvePart2(input: Seq[String]): Unit = {
    val (algo, img) = parseInput(input)

    val enhancedImg = (1 to 50).foldRight((img, Pixel.Dark))((_, t) => {
      val (img, hiddenPixels) = t
      val enhancedImg = img.enhance(algo, hiddenPixels)
      val newHiddenPixels = getNewHiddenPixelValue(algo, hiddenPixels)
      (enhancedImg, newHiddenPixels)
    })._1

    val litPixels = enhancedImg.litPixels

    println(litPixels)
  }

  private def parseInput(input: Seq[String]): (ImageEnhancementAlgorithm, Image) = {
    val algo = ImageEnhancementAlgorithm(input.head.map(Pixel.fromChar))

    val map = input.drop(2).map(l => l.map(Pixel.fromChar)).toIndexedSeq

    val pixels = mutable.Map.empty[(Int, Int), Pixel]

    map.indices.map(x => map(x).indices.map(y => pixels.put((x, y), map(x)(y))))

    val img = Image(pixels)

    (algo, img)
  }
}
