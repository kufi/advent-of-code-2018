package ch.kufi.aoc

import scala.annotation.tailrec

class Day17 extends Challenge[Int, Int] {
  val xRegex = """x=([0-9]+), y=([0-9+]+)..([0-9+]+)""".r
  val yRegex = """y=([0-9]+), x=([0-9+]+)..([0-9+]+)""".r

  override def part1(): Int = {
    val well = Square(500, 0)

    val clay = readLines("day17-test.txt")
      .flatMap {
        case xRegex(x, fromY, toY) =>
          (fromY.toInt to toY.toInt).map(y => Square(x.toInt, y))
        case yRegex(y, fromX, toX) =>
          (fromX.toInt to toX.toInt).map(x => Square(x, y.toInt))
      }
      .toSet

    val maxY = clay.maxBy(_.y).y

    val (_, allWatered) = Iterator
      .iterate((List(Down(well.below), Left(well.left), Right(well.right)), Set.empty[Square])) {
        case (Down(square) :: rest, watered) if square.y > maxY =>
          (rest, watered)
        case (Down(square) :: rest, watered) if !clay.contains(square) && !watered.contains(square) =>
          (Down(square.below) +: Left(square.left) +: Right(square.right) +: rest, watered + square)
        case (Down(square) :: rest, watered) =>
          (rest, watered)
        case (Left(square) :: rest, watered) if !clay.contains(square) && !watered.contains(square) && enclosedInClay(square.below, clay, watered) =>
          if (watered.contains(square.below) || clay.contains(square.below)) {
            (Left(square.left) +: rest, watered + square)
          } else if (clay.contains(square.right.below)) {
            (Down(square.below) +: rest, watered + square)
          } else {
            (rest, watered)
          }
        case (Left(_) :: rest, watered) =>
          (rest, watered)
        case (Right(square) :: rest, watered) if !clay.contains(square) && !watered.contains(square) && enclosedInClay(square.below, clay, watered) =>
          if (watered.contains(square.below) || clay.contains(square.below)) {
            (Right(square.right) +: rest, watered + square)
          } else if (clay.contains(square.left.below)) {
            (Down(square.below) +: rest, watered + square)
          } else {
            (rest, watered)
          }
        case (Right(_) :: rest, watered) =>
          (rest, watered)
      }
      .map(a => {
        printCave(clay, a._2)
        println(a._1.take(5) + " - " + a._1.size)
        println("*******************")
        a
      })
      .find(_._2.count(_.y == maxY) == 2)
      .get

    printCave(clay, allWatered)
    println("***************************************************")

    allWatered.size
  }

  def enclosedInClay(square: Square, clay: Set[Square], watered: Set[Square]): Boolean = {
    nextEnclosed(square.left, clay, watered, _.left) && nextEnclosed(square.right, clay, watered, _.right)
  }

  @tailrec
  private def nextEnclosed(square: Square, clay: Set[Square], watered: Set[Square], next: Square => Square): Boolean = {
    if (clay.contains(square)) return true
    if (!watered.contains(square)) return false
    nextEnclosed(next(square), clay, watered, next)
  }

  def printCave(clay: Set[Square], watered: Set[Square]) = {
    val maxY = (clay ++ watered).maxBy(_.y).y
    val maxX = (clay ++ watered).maxBy(_.x).x

    val minX = clay.minBy(_.x).x

    (0 to maxY).foreach(y => {
      (minX - 1 to maxX).foreach(x => {
        print(if (clay.contains(Square(x, y))) {
          "#"
        } else if (watered.contains(Square(x, y))) {
          "|"
        } else if (x == 500 && y == 0) {
          "+"
        } else {
          "."
        })
      })
      println("")
    }

    )
    println("*****************************************************************************")
  }

  sealed trait Search

  case class Down(square: Square) extends Search

  case class Left(square: Square) extends Search

  case class Right(square: Square) extends Search

  case class Square(x: Int, y: Int) {
    def above: Square = Square(x, y - 1)

    def left: Square = Square(x - 1, y)

    def right: Square = Square(x + 1, y)

    def below: Square = Square(x, y + 1)
  }

  override def part2(): Int = {
    0
  }
}
