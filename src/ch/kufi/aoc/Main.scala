package ch.kufi.aoc

import java.time.LocalDate

object Main {
  val challenges: Map[Int, Challenge] = Map(
    1 -> new Day1()
  )

  def main(args: Array[String]): Unit = {
    val challenge = getChallenge(args)
    println(challenge.map(_.part1()))
    println(challenge.map(_.part2()))
  }

  private def getChallenge(args: Array[String]) = {
    if (args.length == 0) {
      challenges.get(LocalDate.now().getDayOfMonth)
    } else {
      challenges.get(args(0).toInt)
    }
  }
}
