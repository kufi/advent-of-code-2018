package ch.kufi.aoc

import java.util

class Day9 extends Challenge[Long, Long] {
  val inputRegex = """([0-9]+) players; last marble is worth ([0-9]+) points""".r

  override def part1(): Long = {
    val (numPlayers, maxPoints) = readInput()

    scoresOfGame(numPlayers, maxPoints).maxBy(_._2)._2
  }

  override def part2(): Long = {
    val (numPlayers, maxPoints) = readInput()

    scoresOfGame(numPlayers, maxPoints * 100).maxBy(_._2)._2
  }

  private def scoresOfGame(numPlayers: Int, maxPoints: Int) = {
    Stream
      .from(1)
      .take(maxPoints)
      .toList
      .foldLeft((Players(numPlayers, 0, Map.empty), Marble.create(0))) {
        case ((players, circle), marble) if marble % 23 == 0 =>
          val retrievedMarble = circle.previous.previous.previous.previous.previous.previous.previous.remove()

          val updatedPlayers = players
            .giveMarbleToCurrentPlayer(marble)
            .giveMarbleToCurrentPlayer(retrievedMarble.score)
            .movePlayer()

          (updatedPlayers, retrievedMarble.next)
        case ((players, circle), marble) =>
          (players.movePlayer(), circle.next.append(Marble.create(marble)))
      }._1.scores
  }

  case class Players(count: Int, currentPlayer: Int, scores: Map[Int, Long]) {
    def giveMarbleToCurrentPlayer(marble: Int): Players = {
      val playerScore = scores.getOrElse(currentPlayer, 0L)
      val newScores = scores.updated(currentPlayer, playerScore + marble.toLong)
      this.copy(scores = newScores)
    }

    def movePlayer(): Players = {
      copy(currentPlayer = (currentPlayer + 1) % count)
    }
  }

  case class Marble(score: Int, var previous: Marble, var next: Marble) {
    def remove(): Marble = {
      this.previous.next = next
      this.next.previous = previous
      this
    }

    def append(marble: Marble): Marble = {
      marble.previous = this
      marble.next = next
      next.previous = marble
      next = marble
      marble
    }

    override def toString: String = score.toString
  }

  object Marble {
    def create(score: Int): Marble = {
      val zeroMarble = Marble(score, null, null)
      zeroMarble.next = zeroMarble
      zeroMarble.previous = zeroMarble
      zeroMarble
    }
  }

  private def readInput() = {
    readLines("day9.txt")
      .map {
        case inputRegex(numPlayers, maxPoints) =>
          (numPlayers.toInt, maxPoints.toInt)
      }
      .toList
      .head
  }
}
