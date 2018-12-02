package ch.kufi.aoc

class Day2 extends Challenge[Int, String] {
  override def part1(): Int = {
    val charCounts = readLines("day2.txt")
      .map(_
        .groupBy(identity)
        .values
        .map(_.length)
        .toSet
      )
      .foldLeft((0, 0)) {
        case (counts, charCount) =>
          (counts._1 + charCount.count(_ == 2), counts._2 + charCount.count(_ == 3))
      }

    charCounts._1 * charCounts._2
  }

  override def part2(): String = {
    readLines("day2.txt")
      .toList
      .combinations(2)
      .map(tuple => (tuple.head.length - 1, createDiffString(tuple.head, tuple.last)))
      .find(t => t._1 == t._2.length)
      .map(_._2)
      .get
  }

  private def createDiffString(first: String, second: String) = {
    first
      .zip(second)
      .filter(a => a._1 == a._2)
      .foldLeft("")(_ + _._1)
  }
}
