package ch.kufi.aoc

import scala.io.Source

trait Challenge[T1, T2] {
  def part1(): T1

  def part2(): T2

  def readLines(file: String): Iterator[String] = Source.fromResource(file).getLines
}
