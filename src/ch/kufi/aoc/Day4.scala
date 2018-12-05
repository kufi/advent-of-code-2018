package ch.kufi.aoc

import java.time.{Instant, LocalDateTime, ZoneOffset}
import java.time.format.DateTimeFormatter

class Day4 extends Challenge[Int, Int] {

  val shiftBegin = """\[(.*)\] Guard #([0-9]*) begins shift""".r
  val fallsAsleep = """\[(.*)\] falls asleep""".r
  val wakesUp = """\[(.*)\] wakes up""".r

  val format = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm")

  implicit val localDateOrdering: Ordering[LocalDateTime] = Ordering.by(_.toEpochSecond(ZoneOffset.UTC))

  override def part1(): Int = {
    val asleepCycles = createGuardAsleepCycles()
    val max = asleepCycles.maxBy(_.asleepTimes.sumMinutesAsleep())
    max.guardNumber * max.asleepTimes.mostOftenAsleep().minute
  }

  override def part2(): Int = {
    val guardMostOftenAsleep = createGuardAsleepCycles()
      .maxBy(guard => guard.asleepTimes.mostOftenAsleep().times)

    guardMostOftenAsleep.guardNumber * guardMostOftenAsleep.asleepTimes.mostOftenAsleep().minute
  }

  private def createGuardAsleepCycles(): Set[GuardAsleepTimes] = {
    readShifts()
      .foldLeft(EmptyState: ShiftState) {
        case (state, ShiftStart(_, number)) =>
          GuardState(number, state.guardsAsleepTimes)
        case (state: GuardState, FallsAsleep(date)) =>
          GuardAsleepState(state.currentGuard, date, state.guardsAsleepTimes)
        case (state: GuardAsleepState, WakesUp(date)) =>
          state.incrementGuardTimes(date)
      }
      .guardsAsleepTimes
      .map(guardAsleepTimes => GuardAsleepTimes(guardAsleepTimes._1, guardAsleepTimes._2))
      .toSet
  }

  private def readShifts(): Seq[Statement] = {
    readLines("day4.txt")
      .map {
        case shiftBegin(date, guardNumber) =>
          ShiftStart(LocalDateTime.from(format.parse(date)), guardNumber.toInt)
        case fallsAsleep(date) =>
          FallsAsleep(LocalDateTime.from(format.parse(date)))
        case wakesUp(date) =>
          WakesUp(LocalDateTime.from(format.parse(date)))
      }
      .toList
      .sortBy(_.date)
  }

  sealed trait ShiftState {
    def guardsAsleepTimes: Map[Int, AsleepTimes]
  }


  case object EmptyState extends ShiftState {
    override def guardsAsleepTimes: Map[Minute, AsleepTimes] = Map.empty
  }

  case class GuardState(currentGuard: Int, guardsAsleepTimes: Map[Int, AsleepTimes]) extends ShiftState

  case class GuardAsleepState(currentGuard: Int, fellAsleepAt: LocalDateTime, guardsAsleepTimes: Map[Int, AsleepTimes]) extends ShiftState {
    def incrementGuardTimes(wokeUpDate: LocalDateTime): GuardState = {
      val newAsleepTimes = Stream
        .from(fellAsleepAt.getMinute)
        .take(wokeUpDate.getMinute - fellAsleepAt.getMinute)
        .foldLeft(getAsleepTimesForCurrentGuard()) {
          case (asleepTimes, minute) => asleepTimes.incrementMinute(minute)
        }
      GuardState(currentGuard, guardsAsleepTimes + (currentGuard -> newAsleepTimes))
    }

    private def getAsleepTimesForCurrentGuard() = {
      guardsAsleepTimes.getOrElse(currentGuard, AsleepTimes(Map.empty))
    }
  }

  type Minute = Int

  type Times = Int

  case class GuardAsleepTimes(guardNumber: Int, asleepTimes: AsleepTimes)

  case class MostOftenAsleepAt(minute: Minute, times: Times)

  case class AsleepTimes(asleepTimes: Map[Minute, Times]) {
    def incrementMinute(atMinute: Minute) =
      AsleepTimes(asleepTimes + (atMinute -> (asleepTimes.getOrElse(atMinute, 0) + 1)))

    def sumMinutesAsleep() = {
      asleepTimes.foldLeft(0)((sumOfMinutesAsleep, timesAsleepAtMinute) => sumOfMinutesAsleep + timesAsleepAtMinute._2)
    }

    def mostOftenAsleep(): MostOftenAsleepAt = {
      val mostOftenAsleep = asleepTimes.maxBy(asleepAtMinute => asleepAtMinute._2)
      MostOftenAsleepAt(mostOftenAsleep._1, mostOftenAsleep._2)
    }
  }

  sealed trait Statement {
    def date: LocalDateTime
  }

  case class ShiftStart(date: LocalDateTime, guardNumber: Int) extends Statement

  case class FallsAsleep(date: LocalDateTime) extends Statement

  case class WakesUp(date: LocalDateTime) extends Statement

}
