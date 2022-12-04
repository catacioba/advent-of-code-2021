package edu.aoc

import scala.collection.mutable

object Ch12 extends AocSolver {

  override def challenge(): String = "ch12"

  case class Edge(from: String, to: String)

  trait VisitingStrategy {
    def visit(node: String): VisitingStrategy

    def alreadyVisited: Set[String]
  }

  class Graph(val adj: Map[String, Set[String]], visitingStrategy: VisitingStrategy) {
    def paths: Int = {
      def dfs(node: String, target: String, visitingStrategy: VisitingStrategy, path: List[String]): Int = {
        if node == target then {
          1
        } else {
          val neighbors = adj(node).diff(visitingStrategy.alreadyVisited).toSeq
          neighbors.map(n => {
            val newVisited = visitingStrategy.visit(n)
            dfs(n, target, newVisited, n :: path)
          }).sum
        }
      }

      case class DfsStep(node: String, visitingStrategy: VisitingStrategy, path: Vector[String])

      def iterativeDfs(start: String, target: String, visitingStrategy: VisitingStrategy): Int = {
        val stack = mutable.Stack.empty[DfsStep]
        var cnt = 0

        stack.push(DfsStep(start, visitingStrategy.visit(start), Vector(start)))

        while (stack.nonEmpty) {
          val step = stack.pop

          if step.node == target then {
            cnt += 1
          } else {
            for (n <- adj(step.node).diff(step.visitingStrategy.alreadyVisited)) {
              stack.push(DfsStep(n, step.visitingStrategy.visit(n), step.path.appended(n)))
            }
          }
        }

        cnt
      }

      val start = "start"
      val end = "end"
      iterativeDfs(start, end, visitingStrategy)
    }
  }

  class AtMostOnceVisitingStrategy(private val visited: Set[String] = Set.empty) extends VisitingStrategy {

    override def visit(node: String): VisitingStrategy = {
      AtMostOnceVisitingStrategy(if node.head.isLower then visited + node else visited)
    }

    override def alreadyVisited: Set[String] = visited
  }

  class AtMostTwiceVisitingStrategy(private val visited: Set[String] = Set.empty,
                                    private val seen: Set[String] = Set.empty,
                                    private val isSmallVisited: Boolean = false) extends VisitingStrategy {

    override def visit(node: String): VisitingStrategy = {
      if node.head.isUpper then this else {
        if node == "start" || node == "end" then AtMostTwiceVisitingStrategy(visited + node, seen, isSmallVisited)
        else {
          if isSmallVisited then {
            AtMostTwiceVisitingStrategy(visited + node, seen, true)
          }
          else {
            if seen.contains(node) then {
              AtMostTwiceVisitingStrategy(visited ++ seen, seen, true)
            }
            else {
              AtMostTwiceVisitingStrategy(visited, seen + node, false)
            }
          }
        }
      }
    }

    override def alreadyVisited: Set[String] = visited
  }

  object Graph {
    def fromEdges(edges: Seq[Edge], visitingStrategy: VisitingStrategy): Graph = {
      val adj = edges
        .flatMap(e => Seq(e.from -> e.to, e.to -> e.from))
        .foldLeft(Map.empty[String, Set[String]])((m, e) => {
          e match {
            case (to, from) =>
              val neighbors = m.getOrElse(to, Set.empty) + from
              m.updated(to, neighbors)
          }
        })
      Graph(adj, visitingStrategy)
    }
  }

  override def solvePart1(input: Seq[String]): Unit = {
    val edges = parseInput(input)
    val graph = Graph.fromEdges(edges, new AtMostOnceVisitingStrategy())
    println(graph.paths)
  }

  override def solvePart2(input: Seq[String]): Unit = {
    val edges = parseInput(input)
    val graph = Graph.fromEdges(edges, new AtMostTwiceVisitingStrategy())
    println(graph.paths)
  }

  private def parseInput(input: Seq[String]): Seq[Edge] = {
    input.map(l => l.split("-")).map(t => Edge(t(0), t(1)))
  }
}
