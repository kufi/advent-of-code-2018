package ch.kufi.aoc

class Day13 extends Challenge[(Int, Int), (Int, Int)] {
  val cartChars = Map('>' -> Right, '<' -> Left, 'v' -> Down, '^' -> Up)

  override def part1(): (Int, Int) = {
    val (map, initialCarts) = loadMap()

    val firstCrash = runCarts(map, initialCarts)
      .find(_._2.nonEmpty)
      .get

    firstCrash._2.head.position
  }


  override def part2(): (Int, Int) = {
    val (map, initialCarts) = loadMap()

    val lastStanding = runCarts(map, initialCarts)
      .find(_._1.size == 1)
      .get

    lastStanding._1.head.position
  }


  private def runCarts(map: Map[(Int, Int), Char], initialCarts: List[Cart]): Stream[(List[Cart], Set[Cart])] = {
    Stream
      .iterate((initialCarts, Set.empty[Cart])) { case (uncrashedCarts, crashedCarts) =>
        val tick = uncrashedCarts
          .sortWith(_.movesFirst(_))
          .foldLeft(Tick.newTick(uncrashedCarts)) { case (tick, cart) =>
            if (tick.hasCrashedThisTick(cart)) {
              tick.removeFromUnmoved(cart)
            } else {
              tick.updateCart(cart, map)
            }
          }

        (tick.moved.toList, tick.crashedThisTick ++ crashedCarts)
      }
  }

  case class Tick(moved: Set[Cart], unmoved: Set[Cart], crashedThisTick: Set[Cart]) {
    def hasCrashedThisTick(cart: Cart) = crashedThisTick.contains(cart)

    def removeFromUnmoved(cart: Cart): Tick = {
      this.copy(unmoved = unmoved.filter(_ != cart))
    }

    def updateCart(cart: Cart, map: Map[(Int, Int), Char]): Tick = {
      val stillUncrashed = (unmoved ++ moved).filter(!crashedThisTick.contains(_))
      val movedCart = cart.move()

      stillUncrashed
        .find(_.position == movedCart.position)
        .map(crashedWith => {
          Tick(
            moved - crashedWith,
            unmoved - cart - crashedWith,
            crashedThisTick ++ Set(movedCart, crashedWith))
        })
        .getOrElse(Tick(
          moved + movedCart.doTurn(map(movedCart.position)),
          unmoved - cart,
          crashedThisTick))
    }
  }

  case object Tick {
    def newTick(uncrashed: Seq[Cart]) = Tick(Set.empty[Cart], uncrashed.toSet, Set.empty[Cart])
  }

  private def removeCarsFromMap(mapWithCarts: Map[(Int, Int), Char]) = {
    mapWithCarts.mapValues {
      case char if char == '<' || char == '>' => '-'
      case char if char == 'v' || char == '^' => '|'
      case char => char
    }
  }

  private def getCarsFromMap(mapWithCarts: Map[(Int, Int), Char]) = {
    mapWithCarts
      .filter(a => cartChars.contains(a._2))
      .map {
        case (position, direction) =>
          Cart(position, cartChars(direction), LeftTurn)
      }
      .toList
  }

  private def loadMap() = {
    val mapWithCarts = readLines("day13.txt")
      .toList
      .map(_.toCharArray.zipWithIndex)
      .zipWithIndex
      .flatMap {
        case (l, i) =>
          l.map {
            case (m, i2) =>
              (i2, i) -> m
          }
      }
      .toMap


    (removeCarsFromMap(mapWithCarts), getCarsFromMap(mapWithCarts))
  }

  def printMapAndCarts(map: Map[(Int, Int), Char], movedCarts: List[Cart]) = {
    val maxY = map.maxBy(_._1._2)._1._2
    val maxX = map.maxBy(_._1._1)._1._1

    (0 to maxY).foreach(y => {
      (0 to maxX).foreach(x => {
        print(movedCarts
          .find(_.position == (x, y))
          .map(a => '#')
          .orElse(map.get(x, y))
          .getOrElse(' '))
      })
      println("")
    })
    println("*****************************************************************************")
  }

  case class Cart(position: (Int, Int), direction: Direction, turn: Turn) {
    def movesFirst(cart: Cart): Boolean = {
      if (position._2 == cart.position._2) {
        position._1 < cart.position._1
      } else {
        position._2 < cart.position._2
      }
    }

    def doTurn(track: Char): Cart = {
      track match {
        case '/' =>
          if (direction == Left || direction == Right) {
            copy(direction = direction.turnLeft)
          } else {
            copy(direction = direction.turnRight)
          }
        case '-' =>
          this
        case '|' =>
          this
        case '\\' =>
          if (direction == Left || direction == Right) {
            copy(direction = direction.turnRight)
          } else {
            copy(direction = direction.turnLeft)
          }
        case '+' =>
          copy(direction = turn.doTurn(direction), turn = turn.next)
      }
    }

    def move(): Cart = direction match {
      case Up => copy(position = (position._1, position._2 - 1))
      case Down => copy(position = (position._1, position._2 + 1))
      case Left => copy(position = (position._1 - 1, position._2))
      case Right => copy(position = (position._1 + 1, position._2))
    }

  }

  sealed trait Turn {
    def next: Turn

    def doTurn(direction: Direction): Direction
  }

  case object LeftTurn extends Turn {
    override def next: Turn = Straight

    override def doTurn(direction: Direction): Direction = direction.turnLeft
  }

  case object Straight extends Turn {
    override def next: Turn = RightTurn

    override def doTurn(direction: Direction): Direction = direction
  }

  case object RightTurn extends Turn {
    override def next: Turn = LeftTurn

    override def doTurn(direction: Direction): Direction = direction.turnRight
  }

  sealed trait Direction {
    def turnRight: Direction

    def turnLeft: Direction
  }

  case object Up extends Direction {
    override def turnRight: Direction = Right

    override def turnLeft: Direction = Left
  }

  case object Right extends Direction {
    override def turnRight: Direction = Down

    override def turnLeft: Direction = Up
  }

  case object Down extends Direction {
    override def turnRight: Direction = Left

    override def turnLeft: Direction = Right
  }

  case object Left extends Direction {
    override def turnRight: Direction = Up

    override def turnLeft: Direction = Down
  }

}
