package ch.kufi.aoc

class Day12 extends Challenge[Int, Long] {

  override def part1(): Int = {
    val finalConstellation = constellations()
      .take(21)
      .head

    finalConstellation
      .filter(_.hasPlant)
      .map(_.index)
      .sum
  }

  override def part2(): Long = {
    def plantList(a: List[Pot]) = a.map(_.hasPlant)

    val (fixedIteration, atIteration) = constellations()
      .zipWithIndex
      .sliding(2)
      .find(lastTwo => plantList(lastTwo.head._1) == plantList(lastTwo.last._1))
      .get
      .head

    fixedIteration
      .filter(_.hasPlant)
      .map(_.index + 50000000000L - atIteration)
      .sum
  }

  private def constellations() = {
    val (initialPots, notes) = readInput()
    val noteSize = notes.head.size

    Stream
      .iterate(initialPots) { pots =>
        padWithEmptyPots(pots)
          .sliding(noteSize)
          .map(podConstellation =>
            Pot(podConstellation(noteSize / 2).index, notes.contains(podConstellation.map(_.hasPlant)))
          ).toList
      }
  }

  def padWithEmptyPots(pots: List[Pot]): List[Pot] = {
    val firstLastPot = pots
      .dropWhile(!_.hasPlant)
      .reverse
      .dropWhile(!_.hasPlant)
      .reverse

    val firstPot = firstLastPot.head
    val lastPot = firstLastPot.last

    val potsBefore = (firstPot.index - 4 until firstPot.index).map(i => Pot.empty(i)).toList
    val potsAfter = (lastPot.index + 1 to lastPot.index + 4).map(i => Pot.empty(i)).toList
    potsBefore ++ firstLastPot ++ potsAfter
  }

  private def readInput() = {
    val lines = readLines("day12.txt").toList
    val pots = lines.head
    val notes = lines.tail.tail
      .filter(_.last == '#')
      .map(_.takeWhile(_ != ' ').map(_ == '#').toList)
      .toSet

    val initialPots = pots
      .replace("initial state: ", "")
      .toList
      .zipWithIndex
      .map(a => Pot(a._2, a._1 == '#'))
    (initialPots, notes)
  }

  case class Pot(index: Int, hasPlant: Boolean)

  case object Pot {
    def full(i: Int) = Pot(i, true)

    def empty(i: Int) = Pot(i, false)
  }

}
