package ch.kufi.aoc

import scala.io.Source

class Day1 extends Challenge[Int, Int] {
  override def part1(): Int = {
    frequencies.sum
  }

  override def part2(): Int = {
    Stream
      .continually(frequencies.toStream)
      .flatten
      .scanLeft(Set[Int]() -> 0) {
        case ((existingFrequencies, lastFrequency), newFrequency) =>
          (existingFrequencies + lastFrequency) -> (lastFrequency + newFrequency)
      }
      .find(frequencies => frequencies._1.contains(frequencies._2))
      .map(_._2)
      .get
  }

  private val frequencies = {
    Source.fromResource("day1.txt").getLines.map(_.toInt).toList
  }
}
