package ch.kufi.aoc

class Day25 extends Challenge[Int, String] {
  override def part1(): Int = {
    val coordinates = mapCordinates(readLines("day25.txt"))

    val resultingConstellations = coordinates.foldLeft(List.empty[Constellation]) { (constellations, coordinate) =>
      val (matching, nonMatching) = constellations.partition(_.inConstellation(coordinate))
      nonMatching :+ matching.foldLeft(Constellation(Set(coordinate)))(_.combine(_))
    }

    resultingConstellations.size
  }

  private def mapCordinates(input: Iterator[String]) = {
    input.map { c =>
      val splitted = c.split(",").toList
      Coordinate(
        splitted.head.toInt,
        splitted(1).toInt,
        splitted(2).toInt,
        splitted(3).toInt
      )
    }.toList
  }

  override def part2(): String = {
    "Not needed"
  }

  case class Constellation(coordinates: Set[Coordinate]) {
    def inConstellation(coordinate: Coordinate): Boolean = {
      coordinates.exists(_.distanceTo(coordinate) <= 3)
    }

    def combine(constellation: Constellation): Constellation =
      Constellation(coordinates ++ constellation.coordinates)
  }

  case class Coordinate(w: Int, x: Int, y: Int, z: Int) {
    def distanceTo(coordinate: Coordinate): Int = {
      (w - coordinate.w).abs + (x - coordinate.x).abs + (y - coordinate.y).abs + (z - coordinate.z).abs
    }
  }

}
