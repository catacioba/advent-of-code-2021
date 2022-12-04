package edu.aoc

object Ch03 extends AocSolver {

  override def challenge(): String = "ch03"

  case class ColumnStats(ones: Int, zeros: Int) {
    def mostCommon(): Char = {
      if ones >= zeros then '1' else '0'
    }

    def leastCommon(): Char = {
      if ones >= zeros then '0' else '1'
    }
  }

  override def solvePart1(input: Seq[String]): Unit = {
    val rows = input.length
    val columns = input.head.length

    val colStats = (0 until columns).map(columnStats(input, _))

    val epsilon = fromBinary(colStats.map(_.mostCommon()))
    val gamma = fromBinary(colStats.map(_.leastCommon()))

    println(epsilon * gamma)
  }

  override def solvePart2(input: Seq[String]): Unit = {
    val rows = input.length
    val columns = input.head.length

    var col = 0
    var filteredColumns = input
    while (filteredColumns.length > 1) {
      val colStats = columnStats(filteredColumns, col)
      filteredColumns = filteredColumns.filter(s => s.charAt(col) == colStats.mostCommon())
      col += 1
    }
    val oxygen = fromBinary(filteredColumns.head)

    col = 0
    filteredColumns = input
    while (filteredColumns.length > 1) {
      val colStats = columnStats(filteredColumns, col)
      filteredColumns = filteredColumns.filter(s => s.charAt(col) == colStats.leastCommon())
      col += 1
    }
    val co2 = fromBinary(filteredColumns.head)

    println(oxygen * co2)
  }

  private def columnStats(input: Seq[String], column: Int): ColumnStats = {
    val ones = input.count(row =>
                             row.charAt(column) match {
                               case '0' => false
                               case '1' => true
                             })
    val zeros = input.length - ones
    ColumnStats(ones, zeros)
  }

  private def fromBinary(bs: IndexedSeq[Char]): Int = Integer.parseInt(bs.mkString, 2)
}
