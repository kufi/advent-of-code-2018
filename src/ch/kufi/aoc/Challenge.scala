package ch.kufi.aoc

import scala.io.Source

trait Challenge {
  def part1(): String

  def part2(): String

  def readLines(file: String): Iterator[String] = Source.fromResource(file).getLines
}
