package ch.kufi.aoc

class Day10 extends Challenge[String, Int] {
  val starRegex = """position=< *([-0-9]+), *([-0-9]+)> velocity=< *([-0-9]+), *([-0-9]+)>""".r

  override def part1(): String = {
    val starChart = findMessage(loadStars().toList)._1
    createStarChart(starChart)
  }

  private def createStarChart(stars: List[Star]): String = {
    val starPositions = stars.map(_.position).toSet

    val minX = stars.minBy(_.position._1).position._1
    val maxX = stars.maxBy(_.position._1).position._1
    val minY = stars.minBy(_.position._2).position._2
    val maxY = stars.maxBy(_.position._2).position._2

    val positions = for {
      y <- Stream.from(minY).take(maxY - minY + 1)
      x <- Stream.from(minX).take(maxX - minX + 1)
    } yield (x, y)

    positions.foldLeft("\n") {
      case (output, position) =>
        val newOutput = if (starPositions.contains(position)) {
          output + "#"
        } else {
          output + "."
        }

        if (position._1 == maxX) {
          newOutput + "\n"
        } else {
          newOutput
        }
    }
  }

  private def loadStars() = {
    readLines("day10.txt").map {
      case starRegex(x, y, velX, velY) =>
        Star((x.toInt, y.toInt), (velX.toInt, velY.toInt))
    }
  }

  override def part2(): Int = {
    findMessage(loadStars().toList)._2
  }

  private def findMessage(initialStars: List[Star]) = {
    Stream
      .iterate(initialStars)(_.map(_.move()))
      .zipWithIndex
      .find {
        case (stars, _) => {
          val minY = stars.minBy(_.position._2).position._2
          val maxY = stars.maxBy(_.position._2).position._2
          maxY - minY <= 10
        }
      }
      .get
  }

  case class Star(position: (Int, Int), velocity: (Int, Int)) {
    def move(): Star = {
      this.copy(position = (position._1 + velocity._1, position._2 + velocity._2))
    }
  }

}
