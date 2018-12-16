package ch.kufi.aoc

class Day11 extends Challenge[(Int, Int), ((Int, Int), Int)] {
  val serialNumber = 3628

  override def part1(): (Int, Int) = {
    val grid = createGrid(300)
    val powerLevels = createPowerLevels(serialNumber, grid)

    val levels = calculatePowerLevelsUpToSize(3, powerLevels)
    levels(3).maxBy(_._2)._1
  }

  override def part2(): ((Int, Int), Int) = {
    val grid = createGrid(300)
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
    def calculateEven(previousSizes: Map[Int, Map[(Int, Int), Int]]) = {
      val expandGrid = List((0, 0), (size / 2, 0), (0, size / 2), (size / 2, size / 2))
      val lowerGrid = previousSizes(size / 2)

      createGrid(300 - (size - 1))
        .map {
          case (x, y) =>
            (x, y) -> expandGrid.map(pos => lowerGrid((x + pos._1, y + pos._2))).sum
        }
        .toMap
    }

    def calculateOdd(previousSizes: Map[Int, Map[(Int, Int), Int]]) = {
      val half = size / 2
      val expandGridSmall = List((0, 0), (size - half, 0), (0, size - half), (size - half, size - half))
      val smallGrid = previousSizes(half)
      val expandGridBig = List((0, 0), (half, half))
      val bigGrid = previousSizes(size - half)
      val minusBig = List((0, 0), (size - half, size - half))

      createGrid(300 - (size - 1))
        .map {
          case (x, y) =>
            val smallSum = expandGridSmall.map(pos => smallGrid((x + pos._1, y + pos._2))).sum
            val bigSum = expandGridBig.map(pos => bigGrid((x + pos._1, y + pos._2))).sum
            val bigMinus = minusBig.map(pos => smallGrid((x + pos._1, y + pos._2))).sum
            val center = powerLevels((x + half, y + half))
            val cross = bigSum - bigMinus - center

            (x, y) -> (smallSum + cross)
        }
        .toMap
    }

    if (size == 1) {
      Map(size -> powerLevels)
    } else {
      val previousSizes = calculatePowerLevelsUpToSize(size - 1, powerLevels)
      val gridForSize = if (size % 2 == 0) {
        calculateEven(previousSizes)
      } else {
        calculateOdd(previousSizes)
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

  private def createGrid(size: Int): Set[(Int, Int)] = {
    (for {
      y <- 1 to size
      x <- 1 to size
    } yield (x, y)).toSet
  }

  case class Cell(coordinates: (Int, Int), powerLevel: Int)

}
