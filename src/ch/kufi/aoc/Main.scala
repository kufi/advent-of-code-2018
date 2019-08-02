package ch.kufi.aoc

import java.time.LocalDate


object Main {
  val challenges: Map[Int, () => Challenge[_, _]] = Map(
    1 -> (() => new Day1()),
    2 -> (() => new Day2()),
    3 -> (() => new Day3()),
    4 -> (() => new Day4()),
    5 -> (() => new Day5()),
    6 -> (() => new Day6()),
    7 -> (() => new Day7()),
    8 -> (() => new Day8()),
    9 -> (() => new Day9()),
    10 -> (() => new Day10()),
    11 -> (() => new Day11()),
    12 -> (() => new Day12()),
    13 -> (() => new Day13()),
    14 -> (() => new Day14()),
    15 -> (() => new Day15()),
    16 -> (() => new Day16()),
    17 -> (() => new Day17()),
    18 -> (() => new Day18()),
    19 -> (() => new Day19()),
    20 -> (() => new Day20()),
    21 -> (() => new Day21()),
    22 -> (() => new Day22())
  )

  def main(args: Array[String]): Unit = {
    val challenge = getChallenge(args).map(_.apply())
    println(challenge.map(_.part1()).toString)
    println(challenge.map(_.part2()).toString)
  }

  private def getChallenge(args: Array[String]) = {
    if (args.length == 0) {
      challenges.get(LocalDate.now().getDayOfMonth)
    } else {
      challenges.get(args(0).toInt)
    }
  }
}
