package ch.kufi.aoc

import scala.annotation.tailrec

class Day17 extends Challenge[Int, Int] {
  val xRegex = """x=([0-9]+), y=([0-9+]+)..([0-9+]+)""".r
  val yRegex = """y=([0-9]+), x=([0-9+]+)..([0-9+]+)""".r

  override def part1(): Int = {
    val well = Square(500, 0)

    val clay = readClay()
    val allWatered = waterCave(well, clay)

    printCave(clay, allWatered)
    println("***************************************************")

    val minY = clay.minBy(_.y).y
    allWatered.count(_.y >= minY)
  }


  override def part2(): Int = {
    val well = Square(500, 0)

    val clay = readClay()
    val allWatered = waterCave(well, clay)

    val minY = clay.minBy(_.y).y
    allWatered.count(enclosedInClay(_, clay, allWatered))
  }

  private def readClay() = {
    readLines("day17.txt")
      .flatMap {
        case xRegex(x, fromY, toY) =>
          (fromY.toInt to toY.toInt).map(y => Square(x.toInt, y))
        case yRegex(y, fromX, toX) =>
          (fromX.toInt to toX.toInt).map(x => Square(x, y.toInt))
      }
      .toSet
  }

  private def waterCave(well: Square, clay: Set[Square]) = {
    val maxY = clay.maxBy(_.y).y

    Iterator
      .iterate((List(Down(well.below), Left(well.left), Right(well.right)), Set.empty[Square])) {
        case (Down(square) :: rest, watered) if square.y > maxY =>
          (rest, watered)
        case (Down(square) :: rest, watered) if isFree(square, clay, watered) =>
          (Down(square.below) +: Left(square.left) +: Right(square.right) +: rest, watered + square)
        case (Left(square) :: rest, watered) if isFree(square, clay, watered) && shouldExpand(square, clay, watered, square.below.right) =>
          (Down(square.below) +: Left(square.left) +: rest, watered + square)
        case (Right(square) :: rest, watered) if isFree(square, clay, watered) && shouldExpand(square, clay, watered, square.below.left) =>
          (Down(square.below) +: Right(square.right) +: rest, watered + square)
        case (_ :: rest, watered) =>
          (rest, watered)
      }
      .find(_._1.isEmpty)
      .get
      ._2
  }

  /**
    * enclosedInClay(diagonal, clay, watered) checks the following case to pass and generates a Left|Right check (x is the tile being checked, diagonal is y):
    * ..x|||#
    * ..#y||#
    * ..#####
    *
    * isFree && clay.contains checks that the following case passes and generates a Down(..) check (Diagonal is the y, which has to be a clay tile):
    * .x||||#
    * ..y|||#
    * ..#####
    *
    * but fails the following case (y is a watered tile):
    * x|||||#
    * .y#|||#
    * .|#####
    *
    * the clay.contains(square.below) && clay.contains(diagonal) checks that the following case passes (y is a clay tile):
    *
    * ...x|||#
    * #.##y#|#
    * #.#..#|#
    * #.####|#
    * #||||||#
    * ########
    *
    * but the following case fails (y is a watered tile): downstream passes by a clay-wall (without the clay.contains(diagonal) it "spills" onto the clay wall:
    *
    * x|.
    * #y.
    * #|.
    */
  def shouldExpand(square: Square, clay: Set[Square], watered: Set[Square], diagonal: Square): Boolean = {
    enclosedInClay(diagonal, clay, watered) ||
      (isFree(square.below, clay, watered) && clay.contains(diagonal)) ||
      (clay.contains(square.below) && clay.contains(diagonal))
  }

  def isFree(square: Square, clay: Set[Square], watered: Set[Square]): Boolean = {
    !clay.contains(square) && !watered.contains(square)
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

}
