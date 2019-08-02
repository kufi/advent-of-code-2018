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

    val in = parseInput("^EE(WW|SS)EEESWWWS(NEEENWESWWWS|)$")

    println(countAllMoreAwayThan(in, 0))
    countAllMoreAwayThan(parsed, 999)
  }

  def isPalindrom(s: String): Boolean = {
    if (s.length > 0 && s.length % 2 == 0) {
      val parts = s.splitAt(s.length / 2)

      val back = augmentString(parts._2).map {
        case 'N' => 'S'
        case 'S' => 'N'
        case 'W' => 'E'
        case 'E' => 'W'
      }

      back.reverse == parts._1
    } else {
      false
    }
  }

  def inverts(s1: String, s2: String): Boolean = {
    s1.endsWith(s2.splitAt(s2.length / 2)._2)
  }

  def countAllMoreAwayThan(n: Node, moreAwayThan: Int): Int = n match {
    case StringNode(s) if isPalindrom(s) =>
      (moreAwayThan.max(0) - (s.length / 2)).min(0).abs
    case StringNode(s) =>
      (moreAwayThan.max(0) - s.length).min(0).abs
    case ConcatNode(concats) =>
      concats.foldLeft((moreAwayThan, 0, Option.empty[Node])) {
        case ((l, c, Some(StringNode(x))), cn@ChoiceNode(ConcatNode(List(StringNode(y))) :: StringNode("") :: Nil)) if isPalindrom(y) && inverts(x, y) =>
          (l, c, Some(cn))
        case ((l, c, _), node) =>
          val i = findLongest(node)
          (l - i, c + countAllMoreAwayThan(node, l), Some(node))
      }._2
    case ChoiceNode(choices) =>
      choices.map(n => countAllMoreAwayThan(n, moreAwayThan)).sum
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
