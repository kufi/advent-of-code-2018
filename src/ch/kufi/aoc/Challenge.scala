package ch.kufi.aoc

import scala.io.Source

trait Challenge {
  def part1(): Any

  def part2(): Any

  def readLines(file: String): Iterator[String] = Source.fromResource(file).getLines
}
