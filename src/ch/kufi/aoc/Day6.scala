package ch.kufi.aoc

class Day6 extends Challenge[Int, Int] {
  val coordinatesRegex = s"""(.*), (.*)""".r

  val points: List[Point] = loadPoints()
  val coordinates: List[Coordinate] = createCoordinates(points.map(_.x), points.map(_.y))

  override def part1(): Int = {
    val closestPoints = calculateClosestPoints(coordinates, points)
    val infinitePoints = findInfinitePoints(closestPoints, points)
    val closestPointsCount = countCoordinatesClosestToPoints(closestPoints, infinitePoints)
    closestPointsCount.maxBy(_._2)._2
  }

  override def part2(): Int = {
    findPointsBelowSummedDistance(coordinates, points, 10000).size
  }

  def findPointsBelowSummedDistance(coordinates: List[Coordinate], points: List[Point], belowSum: Int): List[Coordinate] = {
    coordinates.filter(_.summedDistanceTo(points) < belowSum)
  }

  private def countCoordinatesClosestToPoints(distancesToPoints: Map[Coordinate, Point], infinitePoints: Set[Point]): Map[Point, Int] = {
    distancesToPoints
      .values
      .filterNot(infinitePoints.contains)
      .groupBy(identity)
      .mapValues(_.size)
  }

  private def findInfinitePoints(distancesToPoints: Map[Coordinate, Point], points: List[Point]): Set[Point] = {
    val x = points.map(_.x)
    val y = points.map(_.y)

    distancesToPoints
      .filterKeys(coordinate => coordinate.x == x.min || coordinate.x == x.max || coordinate.y == y.min || coordinate.y == y.max)
      .values
      .toSet
  }

  private def calculateClosestPoints(coordinates: List[Coordinate], points: List[Point]): Map[Coordinate, Point] = {
    coordinates
      .map(coordinate => coordinate -> coordinate.findClosestPoint(points))
      .toMap
      .filter(_._2.isDefined)
      .mapValues(_.get)
  }

  private def loadPoints(): List[Point] = {
    readLines("day6.txt")
      .map {
        case coordinatesRegex(x, y) => Point(x.toInt, y.toInt)
      }
      .toList
  }

  private def createCoordinates(x: List[Int], y: List[Int]): List[Coordinate] = {
    (for {
      x <- Stream.from(x.min).take(x.max)
      y <- Stream.from(y.min).take(y.max)
    } yield Coordinate(x, y)).toList
  }

  case class Coordinate(x: Int, y: Int) {
    def findClosestPoint(points: List[Point]): Option[Point] = {
      val distances = points.map(point => point.distanceTo(this) -> point)
      val minPointDistance = distances.minBy(_._1)
      if (distances.count(_._1 == minPointDistance._1) == 1) {
        return Some(minPointDistance._2)
      }
      None
    }

    def summedDistanceTo(points: List[Point]): Int = {
      points.foldLeft(0)(_ + _.distanceTo(this))
    }
  }

  case class Point(x: Int, y: Int) {
    def distanceTo(coordinate: Coordinate): Int = {
      (x - coordinate.x).abs + (y - coordinate.y).abs
    }
  }

}
