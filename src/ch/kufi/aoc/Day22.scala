package ch.kufi.aoc

import scala.collection.mutable

class Day22 extends Challenge[Int, Int] {
  val depth = 10914
  val targetX = 9
  val targetY = 739
  val expand = 10

  override def part1(): Int = {
    createMap(targetX, targetY).map(erosionLevel(_) % 3).sum
  }

  override def part2(): Int = {
    Iterator
      .iterate(List(State((0, 0), Torch, 0)), Map.empty[(Int, Int, Tool), Int]) {
        case (state :: tail, visited) if visited.get(state.key).exists(_ <= state.time) || !state.validTool(erosionLevel(state.position) % 3) =>
          (tail, visited)
        case (state :: tail, visited) =>
          val adjacent = state.adjacent()
          val nonToolChanges = adjacent.map(p => State(p, state.tool, state.time + 1))
          val toolChanges = state.tool.others.map(t => State(state.position, t, state.time + 7))
          ((tail ++ nonToolChanges ++ toolChanges).sortBy(_.time), visited + (state.key -> state.time))
      }
      .find(s => s._1.head.position == (targetX, targetY) && s._1.head.tool == Torch)
      .get
      ._1
      .head
      .time
  }

  val cache = mutable.Map.empty[(Int, Int), Int]

  def erosionLevel(coords: (Int, Int)): Int = cache.getOrElseUpdate(coords, {
    coords match {
      case (0, 0) =>
        erosionLevel(0)
      case (`targetX`, `targetY`) =>
        erosionLevel(0)
      case (0, y) =>
        erosionLevel(y * 48271)
      case (x, 0) =>
        erosionLevel(x * 16807)
      case (x, y) =>
        val xIndex = erosionLevel(coords.copy(_1 = x - 1))
        val yIndex = erosionLevel(coords.copy(_2 = y - 1))
        erosionLevel(xIndex * yIndex)
    }
  })

  case class State(position: (Int, Int), tool: Tool, time: Int) {
    def validTool(regionType: Int): Boolean = tool match {
      case Torch => regionType != 1
      case ClimbingGear => regionType != 2
      case Neither => regionType != 0
    }

    def adjacent(): Set[(Int, Int)] = {
      Set(
        (position._1, position._2 - 1),
        (position._1, position._2 + 1),
        (position._1 - 1, position._2),
        (position._1 + 1, position._2)
      ).filter(a => a._1 >= 0 && a._2 >= 0 && a._1 < targetX * expand && a._2 < targetY * expand)
    }

    def key: (Int, Int, Tool) = (position._1, position._2, tool)
  }

  sealed trait Tool {
    def others: List[Tool]
  }

  case object Torch extends Tool {
    override def others: List[Tool] = List(ClimbingGear, Neither)
  }

  case object ClimbingGear extends Tool {
    override def others: List[Tool] = List(Torch, Neither)
  }

  case object Neither extends Tool {
    override def others: List[Tool] = List(ClimbingGear, Torch)
  }

  private def createMap(targetX: Int, targetY: Int) = {
    Stream
      .iterate((0, 0)) {
        case (`targetX`, y) =>
          (0, y + 1)
        case (x, y) =>
          (x + 1, y)
      }
      .takeWhile(a => a._1 != targetX || a._2 != targetY)
      .toList
  }

  private def erosionLevel(geologicIndex: Int) = {
    (geologicIndex + depth) % 20183
  }
}
