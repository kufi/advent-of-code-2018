package ch.kufi.aoc

import java.util.UUID

class Day24 extends Challenge[Int, Int] {
  override def part1(): Int = {
    val (first, second) = Iterator.iterate(readArmies(readLines("day24.txt").toIndexedSeq))(fightRound)
      .dropWhile(a => !a._1.lost && !a._2.lost)
      .next()
    
    first.groups.map(_.units).sum + second.groups.map(_.units).sum
  }

  override def part2(): Int = {
    val (immuneSystem, infection) = readArmies(readLines("day24.txt").toIndexedSeq)

    val boostedWin = Iterator.from(1)
      .map { boost =>
        val boostedImmuneSystem = immuneSystem.boost(boost)
        val fight = Iterator.iterate((boostedImmuneSystem, infection))(fightRound)
        val (first, _) = fight
          .sliding(2)
          .map {
            case last :: now :: _ if last == now =>
              (Army(Seq.empty), Army(Seq.empty))
            case _ :: now :: _ => now
          }
          .dropWhile(a => !a._1.lost && !a._2.lost)
          .next()
        boost -> first
      }
      .dropWhile(_._2.groups.isEmpty)
      .next()

    boostedWin._2.groups.map(_.units).sum
  }

  def fightRound(armies: (Army, Army)) = {
    val (first, second) = armies

    val targetSelections = first.targetSelection(second) ++ second.targetSelection(first)
    val attackOrder = targetSelections.sortBy(_.initiative).reverse

    attackOrder.foldLeft(armies) {
      case ((defendingArmy, attackingArmy), attack) if defendingArmy.groups.exists(_.id == attack.defender) =>
        attackingArmy.groups.find(_.id == attack.attacker)
          .map(a => (defendingArmy.runAttackAgainst(a, attack.defender), attackingArmy))
          .getOrElse((defendingArmy, attackingArmy))
      case ((attackingArmy, defendingArmy), attack) =>
        attackingArmy.groups.find(_.id == attack.attacker)
          .map(a => (attackingArmy, defendingArmy.runAttackAgainst(a, attack.defender)))
          .getOrElse((attackingArmy, defendingArmy))
    }
  }

  def readArmies(lines: IndexedSeq[String]): (Army, Army) = {
    val nonEmpty = lines.filter(!_.isEmpty)
    val (immuneSystem, infections) = nonEmpty.splitAt(nonEmpty.indexOf("Infection:"))

    (parseArmy(immuneSystem.tail), parseArmy(infections.tail))
  }

  def parseArmy(groups: Seq[String]): Army = {
    val inputRegex = """([0-9]+) units each with ([0-9]+) hit points( \(.*\)|) with an attack that does ([0-9]+) ([a-z]+) damage at initiative ([0-9]+)""".r
    val weaknessRegex = """.*weak to ([a-z, ]+).*""".r
    val immuneRegex = """.*immune to ([a-z, ]+).*""".r

    Army(
      groups.map {
        case inputRegex(units, hitPoints, weaknessImmunities, damage, attackType, initiative) =>
          val weaknesses = weaknessImmunities match {
            case weaknessRegex(weaknesses) => weaknesses.split(", ").toSet
            case _ => Set.empty[String]
          }

          val immunities = weaknessImmunities match {
            case immuneRegex(immunities) => immunities.split(", ").toSet
            case _ => Set.empty[String]
          }

          Group(
            UUID.randomUUID(),
            units.toInt,
            hitPoints.toInt,
            weaknesses,
            immunities,
            damage.toInt,
            attackType,
            initiative.toInt)
      }
    )
  }

  case class TargetSelection(attacker: UUID, defender: UUID, initiative: Int)

  case class Army(groups: Seq[Group]) {
    def lost = groups.isEmpty

    def boost(boost: Int): Army = Army(groups.map(g => g.copy(damage = g.damage + boost)))

    def runAttackAgainst(attacker: Group, against: UUID): Army = {
      Army(groups.flatMap {
        case group if group.id == against =>
          val damage = attacker.damageAgainst(group)
          val killedUnits = damage / group.hitPoints
          if (killedUnits >= group.units) {
            None
          } else {
            Some(group.copy(units = group.units - killedUnits))
          }
        case o => Some(o)
      })
    }

    def targetSelection(defenders: Army): List[TargetSelection] = {
      val attackOrder = groups.sortWith {
        case (f, s) if f.effectivePower > s.effectivePower => true
        case (f, s) if f.effectivePower < s.effectivePower => false
        case (f, s) => f.initiative > s.initiative
      }

      attackOrder.foldLeft(List.empty[TargetSelection]) { (selection, attacker) =>
        val validDefenders = defenders.groups.filterNot(g => selection.exists(_.defender == g.id))
        val damagesAgainstDefenders = validDefenders.map(d => d -> attacker.damageAgainst(d))
        if (damagesAgainstDefenders.forall(_._2 == 0)) {
          selection
        } else {
          val defender = damagesAgainstDefenders.sortWith {
            case ((_, a1), (_, a2)) if a1 > a2 => true
            case ((_, a1), (_, a2)) if a1 < a2 => false
            case ((d1, _), (d2, _)) if d1.effectivePower > d2.effectivePower => true
            case ((d1, _), (d2, _)) if d1.effectivePower < d2.effectivePower => false
            case ((d1, _), (d2, _)) => d1.initiative > d2.initiative
          }.head

          TargetSelection(attacker.id, defender._1.id, attacker.initiative) +: selection
        }
      }
    }
  }

  case class Group(id: UUID, units: Int, hitPoints: Int, weaknesses: Set[String], immunities: Set[String], damage: Int, attackType: String, initiative: Int) {
    val effectivePower: Int = units * damage

    def damageAgainst(defender: Group): Int = {
      if (defender.immunities.contains(attackType)) return 0
      val totalDamage = units * damage
      if (defender.weaknesses.contains(attackType)) return totalDamage * 2
      totalDamage
    }
  }

}
