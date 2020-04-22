package ch.kufi.aoc

class Day23 extends Challenge[Int, Int] {
  private val botRegex = """pos=<([-0-9]*),([-0-9]*),([-0-9]*)>, r=([0-9]*)""".r

  override def part1(): Int = {
    val bots = loadBots("day23.txt")
    val strongestBot = bots.maxBy(_.radius)
    bots.count(bot => strongestBot.canReach(bot.center))
  }

  private def loadBots(file: String) = {
    readLines(file)
      .map {
        case botRegex(x, y, z, radius) =>
          Bot(Coordinate(x.toInt, y.toInt, z.toInt), radius.toInt)
      }
      .toList
  }

  override def part2(): Int = {
    val bots = loadBots("day23.txt")

    val diffX = bots.map(_.center.x).max - bots.map(_.center.x).min
    val diffY = bots.map(_.center.y).max - bots.map(_.center.y).min
    val diffZ = bots.map(_.center.z).max - bots.map(_.center.z).min

    val start = Cube(Coordinate(0, 0, 0), Seq(diffX, diffY, diffZ).max)

    val result = Iterator
      .iterate(Seq((start, start.botsInRange(bots)))) { cubes =>
        val maxBotCount = cubes.map(_._2).max
        val (cubesToCheck, others) = cubes.partition(_._2 == maxBotCount)

        cubesToCheck
          .flatMap(_._1.split.map(cube => (cube, cube.botsInRange(bots))))
          .filter(_._2 > 0) ++
          others
      }
      .dropWhile(a => a.forall(_._1.size > 1))
      .next()

    val maxBotCount = result.filter(_._1.size == 1).maxBy(_._2)
    val possibleResults = result.filter(_._2 == maxBotCount._2)
    val minDistance = possibleResults.map(_._1.distanceTo(Coordinate(0, 0, 0))).min
    minDistance
  }

  case class Cube(corner: Coordinate, size: Int) {
    private def dist(x: Int, low: Int, high: Int): Int = {
      if (x < low) return low - x
      if (x > high) return x - high
      0
    }

    def distanceTo(coordinate: Coordinate): Int = {
      dist(coordinate.x, corner.x, corner.x + size - 1) +
        dist(coordinate.y, corner.y, corner.y + size - 1) +
        dist(coordinate.z, corner.z, corner.z + size - 1)
    }

    def botsInRange(bots: List[Bot]): Int = {
      bots.count(bot => {
        distanceTo(bot.center) <= bot.radius
      })
    }

    def split: Seq[Cube] = {
      val splittedSize = if (size % 2 == 0) {
        size / 2
      } else {
        (size / 2) + 1
      }

      Seq(
        Cube(corner, splittedSize),
        Cube(corner.copy(x = corner.x + splittedSize), splittedSize),
        Cube(corner.copy(y = corner.y + splittedSize), splittedSize),
        Cube(corner.copy(z = corner.z + splittedSize), splittedSize),
        Cube(corner.copy(x = corner.x + splittedSize, y = corner.y + splittedSize), splittedSize),
        Cube(corner.copy(x = corner.x + splittedSize, z = corner.z + splittedSize), splittedSize),
        Cube(corner.copy(y = corner.y + splittedSize, z = corner.z + splittedSize), splittedSize),
        Cube(corner.copy(x = corner.x + splittedSize, y = corner.y + splittedSize, z = corner.z + splittedSize), splittedSize)
      )
    }
  }

  case class Coordinate(x: Int, y: Int, z: Int) {
    def distanceTo(coordinates: Coordinate): Int = {
      (x - coordinates.x).abs + (y - coordinates.y).abs + (z - coordinates.z).abs
    }
  }

  case class Bot(center: Coordinate, radius: Int) {
    def canReach(coords: Coordinate): Boolean = {
      this.center.distanceTo(coords) <= radius
    }
  }

}
