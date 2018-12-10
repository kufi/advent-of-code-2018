package ch.kufi.aoc

class Day8 extends Challenge[Int, List[List[Int]]] {

  override def part1(): Int = {
    Stream
      .iterate((0, List[State](Subnode), readEntries())) {
        case (checksum, Subnode :: remainingStates, childNodeCount :: metadataCount :: unparsed) =>
          (checksum, List.fill(childNodeCount)(Subnode) ::: Metadata(metadataCount) :: remainingStates, unparsed)
        case (checksum, Metadata(count) :: remainingStates, unparsed) =>
          val (metadata, remainingUnparsed) = unparsed.splitAt(count)
          (checksum + metadata.sum, remainingStates, remainingUnparsed)
      }
      .find(_._2.isEmpty)
      .head
      ._1
  }

  override def part2(): List[List[Int]] = {
    Stream
      .iterate((List(List.empty[Int]), List[State](Subnode), readEntries())) {
        case (subNodes :: otherNodes, Subnode :: nextStates, 0 :: metadataCount :: unparsed) =>
          val (metadata, remainingUnparsed) = unparsed.splitAt(metadataCount)
          ((subNodes :+ metadata.sum) +: otherNodes, nextStates, remainingUnparsed)
        case (subNodeSums, Subnode :: remainingStates, childNodeCount :: metadataCount :: unparsed) =>
          (List.empty +: subNodeSums, List.fill(childNodeCount)(Subnode) ::: Metadata(metadataCount) :: remainingStates, unparsed)
        case (subNodeSums :: parentSubNodeSums :: nextNodeSums, Metadata(count) :: remainingStates, unparsed) =>
          val (indexes, remainingUnparsed) = unparsed.splitAt(count)

          val subNodeCount = indexes.foldLeft(0) {
            case (sum, index) =>
              sum + subNodeSums.lift(index - 1).getOrElse(0)
          }

          ((parentSubNodeSums :+ subNodeCount) +: nextNodeSums, remainingStates, remainingUnparsed)
      }
      .find(_._2.isEmpty)
      .head
      ._1
  }

  private def readEntries() = {
    readLines("day8.txt").toList.head.split(" ").map(_.toInt).toList
  }

  sealed trait State

  case object Subnode extends State

  case class Metadata(count: Int) extends State

}
