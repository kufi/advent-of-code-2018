package ch.kufi.aoc

import scala.util.parsing.combinator._

class Day20 extends Challenge[Int, Int] with RegexParsers {

  override def part1(): Int = {
    val input = readLines("day20.txt").toList.head
    findLongest(parseInput(input))
  }


  def findLongest(o: Node): Int = o match {
    case StringNode(s) => s.length
    case ConcatNode(concats) =>
      concats.map(findLongest).sum
    case ChoiceNode(nodes) =>
      if (nodes.contains(StringNode(""))) {
        0
      } else {
        nodes.map(findLongest).max
      }
  }

  override def part2(): Int = {
    val input = readLines("day20.txt").toList.head
    val parsed = parseInput(input)
    0
  }

  def parseInput(input: String): Node = {

    def inputRegexNode: Parser[Node] = "^" ~> concatNode <~ "$"

    def concatNode: Parser[ConcatNode] = rep1(regexNode) ^^ ConcatNode

    def emptyNode: Parser[StringNode] = "" ^^^ StringNode("")

    def regexNode: Parser[Node] = (
      "[NESW]+".r ^^ StringNode
        | "(" ~> repsep(concatNode | emptyNode, "|") <~ ")" ^^ ChoiceNode
      )

    parseAll(inputRegexNode, input) match {
      case Success(result, next) => result
      case NoSuccess(msg, next) => throw new RuntimeException(s"Regex parsing error: $msg ($next)")
    }
  }

  sealed trait Node

  case class StringNode(s: String) extends Node

  case class ChoiceNode(choices: List[Node]) extends Node

  case class ConcatNode(concats: List[Node]) extends Node

}
