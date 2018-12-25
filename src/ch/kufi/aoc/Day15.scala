package ch.kufi.aoc

class Day15 extends Challenge[Int, Int] {

  override def part1(): Int = {
    val (area, initialState) = loadGameFromFile()

    val finalGameState = runGameToEnd(area, initialState)

    (finalGameState._2 - 1) * finalGameState._1.units.map(_.hitPoint).sum
  }

  override def part2(): Int = {
    val (area, initialState) = loadGameFromFile()

    val elves = initialState.units.count(_.isInstanceOf[Elve])

    val allElvesSurvive = Iterator
      .from(0)
      .map { increaseAttackPowerBy =>
        val alteredState = initialState.copy(units = initialState.units.map {
          case e: Elve => e.copy(attackPower = e.attackPower + increaseAttackPowerBy)
          case g => g
        })

        val finalGameState = runGameToEnd(area, alteredState)
        (finalGameState._1.units.count(_.isInstanceOf[Elve]), (finalGameState._2 - 1) * finalGameState._1.units.map(_.hitPoint).sum)
      }
      .find(_._1 == elves)
      .get

    allElvesSurvive._2
  }

  private def runGameToEnd(area: Area, initialState: GameState) = {
    Iterator
      .iterate(initialState)(_.runRound(area))
      .zipWithIndex
      .find(state => state._1.units.forall(_.isInstanceOf[Goblin]) || state._1.units.forall(_.isInstanceOf[Elve]))
      .head
  }

  def printState(alive: List[GameUnit], area: Area) = {
    val maxY = area.passableFields.maxBy(_.y).y
    val maxX = area.passableFields.maxBy(_.x).x

    (0 to maxY).foreach(y => {
      (0 to maxX).foreach(x => {
        print(alive
          .find(_.position == Field(x, y))
          .map {
            case _: Elve => 'E'
            case _: Goblin => 'G'
          }
          .getOrElse(if (area.passableFields.contains(Field(x, y))) '.' else '#'))
      })
      println("")
    })
    println("*****************************************************************************")
  }

  def loadGameFromFile(): (Area, GameState) = {
    val loadedMap = readLines("day15.txt")
      .toList
      .map(_.split(""))
      .zipWithIndex
      .flatMap(line => line._1
        .zipWithIndex
        .map(field => (field._2, line._2) -> field._1)
      )
      .toSet

    val passableFields = Set(".", "G", "E")

    (
      Area(loadedMap.filter(field => passableFields.contains(field._2)).map(f => Field(f._1._1, f._1._2))),
      GameState(
        loadedMap.filter(field => field._2 == "E").map(f => Elve(200, Field(f._1._1, f._1._2), 3)).toList ++
          loadedMap.filter(field => field._2 == "G").map(f => Goblin(200, Field(f._1._1, f._1._2)))
      )
    )
  }

  case class Field(x: Int, y: Int) {
    def adjacentFields = List(
      this.copy(y = y - 1),
      this.copy(x = x - 1),
      this.copy(x = x + 1),
      this.copy(y = y + 1)
    )

    def moveOrder(position: Field): Boolean = {
      if (y == position.y) {
        x < position.x
      } else {
        y < position.y
      }
    }
  }

  case class MoveState(unmoved: List[GameUnit], moved: List[GameUnit]) {
    def moveNextUnit(area: Area): MoveState = {
      val currentUnit = unmoved.head
      val unmovedUnits = unmoved.tail
      val withoutCurrentUnit = MoveState(unmovedUnits, moved)

      currentUnit.attack(alive)
        .map(attackedUnit => withoutCurrentUnit.updateWithAttackedUnit(currentUnit, attackedUnit._1, attackedUnit._2))
        .getOrElse({
          val movedUnit = currentUnit.doMove(area, alive)
          movedUnit.attack(alive)
            .map(attackedUnit => withoutCurrentUnit.updateWithAttackedUnit(movedUnit, attackedUnit._1, attackedUnit._2))
            .getOrElse(MoveState(unmovedUnits, movedUnit +: moved))
        })
    }

    def alive: List[GameUnit] = unmoved ++ moved

    private def updateWithAttackedUnit(attackingUnit: GameUnit, originalAttackedUnit: GameUnit, attackedUnit: GameUnit): MoveState = {
      if (attackedUnit.isDead) {
        MoveState(unmoved.filterNot(_ == originalAttackedUnit), (moved :+ attackingUnit).filterNot(_ == originalAttackedUnit))
      } else {
        val unmovedIndex = unmoved.indexOf(originalAttackedUnit)
        val newUnmoved = if (unmovedIndex != -1) {
          unmoved.updated(unmovedIndex, attackedUnit)
        } else {
          unmoved
        }

        val movedIndex = moved.indexOf(originalAttackedUnit)
        val newMoved = if (movedIndex != -1) {
          attackingUnit +: moved.updated(movedIndex, attackedUnit)
        } else {
          attackingUnit +: moved
        }

        MoveState(newUnmoved, newMoved)
      }
    }
  }

  case class Area(passableFields: Set[Field])

  case class GameState(units: List[GameUnit]) {
    def moveOrder = units
      .sortWith((a, b) => a.position.moveOrder(b.position))

    def runRound(area: Area): GameState = {
      val aliveUnitsAfterRound = Iterator
        .iterate(MoveState(moveOrder, List.empty))(_.moveNextUnit(area))
        .find(_.unmoved.isEmpty)
        .head
        .alive

      GameState(aliveUnitsAfterRound)
    }
  }

  sealed trait GameUnit {
    def doMove(area: Area, otherUnits: List[GameUnit]): GameUnit = {
      val moveToField = Stream
        .iterate((position.adjacentFields.map(f => (f, f)), Set.empty[Field])) {
          case (field :: rest, visited) if area.passableFields.contains(field._1) && !otherUnits.exists(_.position == field._1) =>
            (rest ++ field._1.adjacentFields.filter(f => !visited.contains(f) && !rest.exists(r => r._1 == f)).map(f => (f, field._2)), visited + field._1)
          case (field :: rest, visited) =>
            (rest, visited + field._1)
        }
        .find(positions => {
          positions._1.isEmpty ||
            otherUnits.exists(unit => unit.position == positions._1.head._1 && canAttack(unit))
        })
        .flatMap(_._1.headOption.map(_._2))

      moveToField.map(pos => this.moveTo(pos)).getOrElse(this)
    }


    def hitPoint: Int

    def attackPower: Int

    def position: Field

    def canAttack(unit: GameUnit): Boolean

    def isDead: Boolean = hitPoint <= 0

    def hit(attackPower: Int): GameUnit

    def moveTo(pos: Field): GameUnit

    def attackOrder(unit: GameUnit): Boolean = {
      if (unit.hitPoint < hitPoint) return false
      if (unit.hitPoint > hitPoint) return true
      position.moveOrder(unit.position)
    }

    def attack(otherUnits: List[GameUnit]): Option[(GameUnit, GameUnit)] = {
      val possibleUnitsToAttack = position
        .adjacentFields
        .flatMap(pos => otherUnits.filter(unit => unit.position == pos && canAttack(unit)))

      possibleUnitsToAttack
        .sortWith(_.attackOrder(_))
        .headOption
        .map(attackedUnit => (attackedUnit, attackedUnit.hit(attackPower)))
    }
  }

  case class Elve(hitPoint: Int, position: Field, attackPower: Int) extends GameUnit {
    def canAttack(unit: GameUnit) = unit match {
      case _: Goblin => true
      case _ => false
    }

    override def hit(attackPower: Int): Elve = this.copy(hitPoint - attackPower)

    override def moveTo(pos: Field): Elve = this.copy(position = pos)
  }

  case class Goblin(hitPoint: Int, position: Field) extends GameUnit {
    val attackPower: Int = 3

    def canAttack(unit: GameUnit) = unit match {
      case _: Elve => true
      case _ => false
    }

    override def hit(attackPower: Int): Goblin = this.copy(hitPoint - attackPower)

    override def moveTo(pos: Field): Goblin = this.copy(position = pos)
  }

}
