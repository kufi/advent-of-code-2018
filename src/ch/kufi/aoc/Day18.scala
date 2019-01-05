package ch.kufi.aoc

class Day18 extends Challenge[Int, Int] {
  override def part1(): Int = {
    val startingArea = loadStartingArea()

    val endState = runSimulation(startingArea)
      .drop(10)
      .next()

    calculateResourceValue(endState)
  }

  override def part2(): Int = {
    val startingArea = loadStartingArea()

    val (_, areaList) = runSimulation(startingArea)
      .drop(1) //drop the initial state, or else we're off by 1
      .zipWithIndex
      .scanLeft((Set.empty[Map[(Int, Int), State]], List.empty[Map[(Int, Int), State]])) {
        case (a, b) =>
          val list = a._2
          (a._1 + b._1, b._1 +: list)
      }
      .find(a => a._1.size != a._2.size)
      .get

    val startLoopElement = areaList.head
    val loop = areaList.reverse.dropWhile(_ != startLoopElement).init

    val toLoop = 1000000000 - (areaList.size - loop.size)
    val endState = loop(toLoop % loop.size)

    calculateResourceValue(endState)
  }

  def printCave(s: Map[(Int, Int), State]) = {
    val maxY = s.maxBy(_._1._2)._1._2
    val maxX = s.maxBy(_._1._1)._1._1

    (0 to maxY).foreach(y => {
      (0 to maxX).foreach(x => {
        print(s((x, y)) match {
          case Trees => '|'
          case Open => '.'
          case Lumberyard => '#'
        })
      })
      println("")
    })
    println("*****************************************************************************")
  }


  private def calculateResourceValue(endState: Map[(Int, Int), State]) = {
    endState.count(_._2 == Trees) * endState.count(_._2 == Lumberyard)
  }

  private def runSimulation(startingArea: Map[(Int, Int), State]) = {
    Iterator
      .iterate(startingArea) { area =>
        area.map {
          case (coordinate, Open) if surroundings(coordinate, area).count(_ == Trees) >= 3 =>
            coordinate -> Trees
          case (coordinate, Trees) if surroundings(coordinate, area).count(_ == Lumberyard) >= 3 =>
            coordinate -> Lumberyard
          case (coordinate, Lumberyard) =>
            val s = surroundings(coordinate, area)
            if (s.contains(Lumberyard) && s.contains(Trees)) {
              coordinate -> Lumberyard
            } else {
              coordinate -> Open
            }
          case unchanged =>
            unchanged
        }
      }
  }

  private def loadStartingArea() = {
    readLines("day18.txt")
      .zipWithIndex
      .flatMap(line => line._1.toCharArray.zipWithIndex.map {
        case ('.', x) =>
          (x, line._2) -> Open
        case ('|', x) =>
          (x, line._2) -> Trees
        case ('#', x) =>
          (x, line._2) -> Lumberyard
      })
      .toMap
  }

  def surroundings(coordinate: (Int, Int), area: Map[(Int, Int), State]) = {
    val adjacent = for {
      xOffset <- -1 to 1
      yOffset <- -1 to 1 if xOffset != 0 || yOffset != 0
    } yield (coordinate._1 + xOffset, coordinate._2 + yOffset)

    adjacent.flatMap(area.get)
  }

  sealed trait State

  case object Open extends State

  case object Trees extends State

  case object Lumberyard extends State

}
