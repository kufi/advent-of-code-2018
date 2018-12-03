package ch.kufi.aoc

class Day3 extends Challenge[Int, Option[Int]] {
  type Overlaps = Map[(Int, Int), Boolean]
  val EmptyOverlaps = Map.empty[(Int, Int), Boolean]

  override def part1(): Int = overlappingSquares().size

  override def part2(): Option[Int] = {
    val overlapping = overlappingSquares().toSet

    claims()
      .find(!_.overlaps(overlapping))
      .map(_.id)
  }

  private def overlappingSquares(): Iterable[(Int, Int)] = {
    claims()
      .foldLeft(EmptyOverlaps) {
        case (overlaps, claim) => claim.addToOverlaps(overlaps)
      }
      .filter(_._2)
      .keys
  }

  private def claims() = {
    readLines("day3.txt")
      .map(line => {
        val claim = line.split(" |@|,|:|x|#")
        Claim(claim(1).toInt, claim(4).toInt, claim(5).toInt, claim(7).toInt, claim(8).toInt)
      })
  }

  case class Claim(id: Int, left: Int, top: Int, width: Int, height: Int) {
    val area: Set[(Int, Int)] = for {
      x <- Stream.from(left).take(width).toSet[Int]
      y <- Stream.from(top).take(height).toSet[Int]
    } yield (x, y)

    def addToOverlaps(overlaps: Overlaps): Overlaps = {
      area
        .foldLeft(overlaps)((existingOverlaps, claim) => {
          existingOverlaps + (claim -> existingOverlaps.contains(claim))
        })
    }

    def overlaps(squares: Set[(Int, Int)]): Boolean = area.exists(squares.contains)
  }

}
