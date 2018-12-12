package ch.kufi.aoc

class Day8 extends Challenge[(Int, Int), (Int, List[List[Int]])] {

  override def part1(): (Int, Int) = {
    val tree = createTree(readEntries())._2

    val iterativeSum = Stream
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

    (tree.sumMetadata, iterativeSum)
  }

  def createTree(entries: List[Int]): (List[Int], Node) = {
    val (entry, unparsed) = entries.splitAt(2)
    val subnodeCount = entry.head
    val metadataCount = entry.last

    if(subnodeCount == 0) {
      val (metadata, restUnparsed) = unparsed.splitAt(metadataCount)
      (restUnparsed, Node(List.empty, metadata))
    } else {
      val (restUnparsed, subnodes) = Stream
        .iterate((unparsed, List.empty[Node])) {
          case (unparsed, subnodes) =>
            val (u, n) = createTree(unparsed)
            (u, subnodes :+ n)
        }
        .drop(subnodeCount)
        .head

      val (metadata, restU) = restUnparsed.splitAt(metadataCount)

      (restU, Node(subnodes, metadata))
    }
  }

  case class Node(subnodes: List[Node], metadata: List[Int]) {
    def sumMetadata: Int = {
      subnodes.foldLeft(0)(_ + _.sumMetadata) + metadata.sum
    }

    def nodeValue: Int = {
      if(subnodes.isEmpty) {
        metadata.sum
      } else {
        metadata.foldLeft(0) {
          case (sum, index) =>
            sum + subnodes.lift(index - 1).map(_.nodeValue).getOrElse(0)
        }
      }
    }
  }

  override def part2(): (Int, List[List[Int]]) = {
    val tree = createTree(readEntries())._2

    val iterativeSum = Stream
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

    (tree.nodeValue, iterativeSum)
  }

  private def readEntries() = {
    readLines("day8.txt").toList.head.split(" ").map(_.toInt).toList
  }

  sealed trait State

  case object Subnode extends State

  case class Metadata(count: Int) extends State

}
