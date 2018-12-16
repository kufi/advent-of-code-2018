package ch.kufi.aoc

class Day11 extends Challenge[(Int, Int), ((Int, Int), Int)] {
  val serialNumber = 3628

  override def part1(): (Int, Int) = {
    val grid = createGrid()
    val powerLevels = createPowerLevels(serialNumber, grid)

    val levels = calculatePowerLevelsUpToSize(3, powerLevels)
    levels(3).maxBy(_._2)._1
  }

  override def part2(): ((Int, Int), Int) = {
    val grid = createGrid()
    val powerLevels = createPowerLevels(serialNumber, grid)

    val levels = calculatePowerLevelsUpToSize(300, powerLevels)
    val maxValuePerLevel = levels.mapValues(_.maxBy(_._2))
    val maxLevel = maxValuePerLevel.maxBy(_._2._2)
    val a = maxLevel._2
    a._1 -> maxLevel._1
  }

  private def createPowerLevels(serialNumber: Int, grid: Set[(Int, Int)]) = {
    grid
      .map(coordinates => {
        coordinates -> calculatePowerLevel(serialNumber, coordinates)
      })
      .toMap
  }

  private def calculatePowerLevelsUpToSize(size: Int, powerLevels: Map[(Int, Int), Int]): Map[Int, Map[(Int, Int), Int]] = {
    if (size == 1) {
      Map(size -> powerLevels)
    } else {
      val previousSizes = calculatePowerLevelsUpToSize(size - 1, powerLevels)
      val removeFrom = 300 - size - 1
      val expandX = (0 until size - 1).map((_, size - 1))
      val expandY = (0 until size - 1).map((size - 1, _))
      val expandGrid = Set(expandX, expandY, Set((size - 1, size - 1))).flatten

      val gridForSize = previousSizes(size - 1)
        .filterKeys(position => position._1 < removeFrom && position._2 < removeFrom)
        .map {
          case (position, powerLevel) =>
            position -> expandGrid.foldLeft(powerLevel) {
              case (sum, pos) =>
                sum + powerLevels((position._1 + pos._1, position._2 + pos._2))
            }
        }

      previousSizes + (size -> gridForSize)
    }
  }

  private def maxForGridSize(grid: Set[(Int, Int)], powerLevels: Map[(Int, Int), Int], size: Int) = {
    val expandSize = size - 1
    val removeFrom = 300 - expandSize
    val expandGrid = for {
      xPlus <- 0 to expandSize
      yPlus <- 0 to expandSize
    } yield (xPlus, yPlus)

    grid
      .filter(position => position._1 < removeFrom && position._2 < removeFrom)
      .map(position => position -> {
        expandGrid.foldLeft(0) {
          case (sum, pos) =>
            sum + powerLevels((position._1 + pos._1, position._2 + pos._2))
        }
      })
      .maxBy(_._2)
  }

  private def calculatePowerLevel(serialNumber: Int, coordinates: (Int, Int)) = {
    val rackId = coordinates._1 + 10
    val powerLevel = ((rackId * coordinates._2) + serialNumber) * rackId
    val finalLevel = if (powerLevel < 100) {
      -5
    } else {
      powerLevel.toString.reverse.charAt(2).asDigit - 5
    }
    finalLevel
  }

  private def createGrid(): Set[(Int, Int)] = {
    (for {
      y <- 1 to 300
      x <- 1 to 300
    } yield (x, y)).toSet
  }

  case class Cell(coordinates: (Int, Int), powerLevel: Int)

}
