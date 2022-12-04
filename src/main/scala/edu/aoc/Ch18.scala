package edu.aoc

import scala.annotation.tailrec
import scala.collection.mutable

object Ch18 extends AocSolver {

  override def challenge(): String = "ch18"

  type ElementId = Int

  object IdGenerator {
    private var idCounter = 0;

    def nextId: ElementId = {
      idCounter += 1
      idCounter
    }
  }

  enum Element:
    case Number(id: ElementId, n: Int)
    case Pair(id: ElementId, left: Element, right: Element)

    def +(that: Element): Element = Element.Pair(IdGenerator.nextId, this, that).reduce

    @tailrec
    private def reduce: Element = {
      val (exploded, hasExploded) = this.explode
      if hasExploded then exploded.reduce
      else {
        val (splitElement, hasSplit) = exploded.split
        if hasSplit then splitElement.reduce else splitElement
      }
    }

    private def update(id: ElementId, el: Element): Element = {
      this match {
        case e@Element.Number(i, _) => if i == id then el else e
        case e@Element.Pair(i, l, r) => if i == id then el else Element.Pair(i, l.update(id, el), r.update(id, el))
      }
    }

    private def inOrder: Seq[Element.Number] = {
      val q = mutable.Queue.empty[Element.Number]

      def inOrderAux(e: Element): Unit = e match {
        case n@Element.Number(_, _) => q.enqueue(n)
        case Element.Pair(id, left, right) =>
          inOrderAux(left)
          inOrderAux(right)
      }

      inOrderAux(this)

      q.toSeq
    }

    private def neighbors(lid: ElementId, rid: ElementId): (Option[Element], Option[Element]) = {
      val inOrderTraversal = inOrder

      val lidx = inOrderTraversal.indexWhere(el => el.id == lid)
      val ridx = inOrderTraversal.indexWhere(el => el.id == rid)

      (if lidx == 0 then None else Some(inOrderTraversal(lidx - 1)),
        if ridx == inOrderTraversal.length - 1 then None else Some(inOrderTraversal(ridx + 1)))
    }

    private def elementToExplode: Option[Element] = {
      def elementToExplodeAux(e: Element, depth: Int): Option[Element] = {
        e match {
          case Element.Number(id, n) => None
          case e@Element.Pair(id, left, right) =>
            if Element.isNumber(left) && Element.isNumber(right) && depth >= 4 then Some(e) else {
              val l = elementToExplodeAux(left, depth + 1)
              if l.isDefined then l else elementToExplodeAux(right, depth + 1)
            }
        }
      }

      elementToExplodeAux(this, 0)
    }

    private def explode: (Element, Boolean) = {
      elementToExplode match {
        case ex@Some(Element.Pair(id, Element.Number(lid, l), Element.Number(rid, r))) =>
          val (ln, rn) = neighbors(lid, rid)

          var updated = this
          ln match {
            case Some(Element.Number(i, n)) => updated = updated.update(i, Element.Number(i, n + l))
            case None =>
          }
          rn match {
            case Some(Element.Number(i, n)) => updated = updated.update(i, Element.Number(i, n + r))
            case None =>
          }

          updated = updated.update(id, Element.Number(id, 0))

          (updated, true)
        case None => (this, false)
      }
    }

    private def elementToSplit: Option[Element] = {
      this match {
        case n@Number(i, x) => if x >= 10 then Some(n) else None
        case Element.Pair(id, left, right) =>
          val l = left.elementToSplit
          if l.isDefined then l else right.elementToSplit
      }
    }

    private def split: (Element, Boolean) = {
      elementToSplit match {
        case Some(n@Number(i, x)) =>
          val updated = this.update(i, Pair(IdGenerator.nextId,
                                            Number(IdGenerator.nextId, x / 2),
                                            Number(IdGenerator.nextId,
                                                   (x + 1) / 2)))
          (updated, true)
        case None => (this, false)
      }
    }

    def magnitude: Int = {
      this match {
        case Element.Number(_, n) => n
        case Element.Pair(_, l, r) => 3 * l.magnitude + 2 * r.magnitude
      }
    }

    override def toString: String = this match {
      case Element.Number(_, n) => n.toString
      case Element.Pair(_, l, r) => s"[$l,$r]"
    }
  end Element

  object Element {
    private def isNumber(e: Element): Boolean = e match {
      case Element.Number(_, _) => true
      case _ => false
    }

    private def isPairOfNumbers(e: Element): Boolean = e match {
      case Element.Pair(_, l, r) if isNumber(l) && isNumber(r) => true
      case _ => false
    }

    private def readUntil(s: String): (String, String) = {
      val i = s.indexWhere(c => c == ',' || c == ']')
      (s.substring(0, i), s.substring(i))
    }

    def fromString(s: String): Element = {
      def readElement(s: String): (Element, String) = {
        s.head match {
          case '[' =>
            val (left, lrem) = readElement(s.drop(1)) // drop '['
            val (right, rrem) = readElement(lrem.drop(1)) // drop ','
            (Element.Pair(IdGenerator.nextId, left, right), rrem.drop(1)) // drop ']'
          case c if c.isDigit =>
            val (num, rem) = readUntil(s)
            (Element.Number(IdGenerator.nextId, num.toInt), rem)
        }
      }

      readElement(s)._1
    }

  }

  override def solvePart1(input: Seq[String]): Unit = {
    val elements = parseInput(input)

    val res = elements.reduceLeft((a, b) => a + b)
    println(res.magnitude)
  }

  override def solvePart2(input: Seq[String]): Unit = {
    val elements = parseInput(input)

    val maxMagnitude = (for (
      x <- elements;
      y <- elements if x != y
    ) yield (x + y).magnitude)
      .max

    println(maxMagnitude)
  }

  private def parseInput(input: Seq[String]): Seq[Element] = {
    input.map(s => Element.fromString(s))
  }
}