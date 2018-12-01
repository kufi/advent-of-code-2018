package ch.kufi.aoc

import scala.io.Source

class Day1 extends Challenge {
  override def part1(): String = {
    frequencies.sum.toString
  }

  override def part2(): String = {
    Stream
      .continually(frequencies.toStream)
      .flatten
      .scanLeft(Set[Int]() -> 0) {
        case ((existingFrequencies, lastFrequency), newFrequency) =>
          (existingFrequencies + lastFrequency) -> (lastFrequency + newFrequency)
      }
      .dropWhile(frequencies => !frequencies._1.contains(frequencies._2))
      .head
      ._2
      .toString
  }

  private val frequencies = {
    Source.fromResource("day1.txt").getLines.map(_.toInt).toList
  }
}
