package ch.kufi.aoc

class Day5 extends Challenge[Int, Int] {
  override def part1(): Int = {
    removeDuplicates(readLines("day5.txt").toList.head).length
  }

  override def part2(): Int = {
    val polymer = readLines("day5.txt").toList.head

    polymer.toLowerCase
      .toCharArray
      .toSet
      .map((u: Char) => removeDuplicates(polymer.replaceAll(s"$u|${u.toString.toUpperCase()}", "")).length)
      .min
  }

  private def removeDuplicates(polymer: String) = {
    polymer
      .toCharArray
      .foldRight(Seq.empty[Char]) {
        case (char, Seq()) =>
          Seq(char)
        case (char, duplicate :: deduplicatedString) if duplicate.toUpper == char.toUpper && duplicate != char =>
          deduplicatedString
        case (char, seq) => char +: seq
      }
      .foldLeft("")(_ + _)
  }
}
