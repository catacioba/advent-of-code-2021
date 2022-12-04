package edu.aoc

import scala.collection.mutable

object Ch14 extends AocSolver {

  override def challenge(): String = "ch14"

  override def solvePart1(input: Seq[String]): Unit = {
    val (template, rules) = parseInput(input)

    var newTemplate = template
    (0 until 10).foreach(_ => {
      newTemplate = step(newTemplate, rules)
    })

    val charFreq = newTemplate.toCharArray.groupMapReduce(identity)(_ => 1L)(_ + _)

    val minFreq = charFreq.values.min
    val maxFreq = charFreq.values.max

    println(maxFreq - minFreq)
  }

  private def step(template: String, rules: Map[(Char, Char), Char]): String = {
    template.zip(template.drop(1)).map((l, r) => s"$l${rules.getOrElse((l, r), "")}").mkString + template.last
  }

  override def solvePart2(input: Seq[String]): Unit = {
    val (template, rules) = parseInput(input)

    var pairCounts = mutable.Map.empty[(Char, Char), Long]

    template.zip(template.drop(1)).foreach(k => incrementCount(pairCounts, k))

    (0 until 40).foreach(_ => {
      pairCounts = freqStep(template, rules, pairCounts)
    })

    val charFreq = pairCounts.toSeq.flatMap {
      case ((l, r), c) => Seq((l, c), (r, c))
    }.groupMapReduce(_._1)(_._2)(_ + _).to(mutable.Map)

    val firstChar = template.head
    val lastChar = template.last

    incrementCount(charFreq, firstChar)
    incrementCount(charFreq, lastChar)

    val minFreq = charFreq.values.min / 2
    val maxFreq = charFreq.values.max / 2

    println(maxFreq - minFreq)
  }

  private def freqStep(template: String,
                       rules: Map[(Char, Char), Char],
                       pairCounts: mutable.Map[(Char, Char), Long]): mutable.Map[(Char, Char), Long] = {
    val newPairCounts = mutable.Map.empty[(Char, Char), Long]

    pairCounts.foreach {
      case (k, c) =>
        if rules.contains(k) then {
          val m = rules(k)
          val a = (k._1, m)
          val b = (m, k._2)
          incrementCount(newPairCounts, a, c)
          incrementCount(newPairCounts, b, c)
        } else {
          incrementCount(newPairCounts, k, c)
        }
    }

    newPairCounts
  }

  private def incrementCount[K](m: mutable.Map[K, Long], key: K, count: Long): Unit = {
    m.updateWith(key) {
      case Some(value) => Some(value + count)
      case None => Some(count)
    }
  }

  private def incrementCount[K](m: mutable.Map[K, Long], key: K): Unit = {
    incrementCount(m, key, 1)
  }

  private def parseInput(input: Seq[String]): (String, Map[(Char, Char), Char]) = {
    val sepIndex = input.indexWhere(x => x.isBlank)

    val (template, rules) = input.splitAt(sepIndex)

    (
      template.head,
      rules.drop(1).map(l => {
        val tokens = l.split("->")
        val lhs = tokens(0).trim
        val rhs = tokens(1).trim
        (lhs(0), lhs(1)) -> rhs(0)
      }).toMap
    )
  }

}
