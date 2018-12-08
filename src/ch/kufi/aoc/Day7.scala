package ch.kufi.aoc

class Day7 extends Challenge[String, Int] {
  val lineRegex = """Step ([A-Z]) must be finished before step ([A-Z]) can begin.""".r

  override def part1(): String = {
    Stream
      .iterate(("", createAllSteps(readSteps()))) {
        case (stepOrder, unfinishedSteps) =>
          val readyStep = unfinishedSteps.filter(_._2.isEmpty).keys.toList.minBy(identity)
          val remainingSteps = unfinishedSteps.filterKeys(_ != readyStep)
          (stepOrder + readyStep, remainingSteps.mapValues(_.filter(_ != readyStep)))
      }
      .find(_._2.isEmpty)
      .get
      ._1
  }

  override def part2(): Int = {
    val numberOfWorkers = 5
    val workers = Array.fill(numberOfWorkers)(Option.empty[Work])

    Stream
      .iterate(TickState(0, workers, createAllSteps(readSteps())))(_.updateTick())
      .takeWhile(!_.isFinished())
      .foldLeft(0)(_ + _.secondsInTick)
  }

  def timeForStep(step: String): Int = {
    val char = step.toCharArray
    (char(0).toInt - 64) + 60
  }

  case class TickState(secondsInTick: Int, workSlots: Array[Option[Work]], unfinishedSteps: Map[String, Set[String]]) {
    def isFinished() = unfinishedSteps.isEmpty && workSlots.forall(_.isEmpty)

    def updateTick(): TickState = {
      val (finishedWorkers, remainingWorkers) = finishedAndRemainingWorkers()

      val finishedSteps = finishedWorkers.map(_.map(_.step)).filter(_.nonEmpty).map(_.get)
      val updatedSteps = unfinishedSteps.mapValues(_.filterNot(finishedSteps.contains))

      val readySteps = updatedSteps.filter(_._2.isEmpty).keys.toList.sortBy(identity)
      val remainingSteps = updatedSteps.filterKeys(!readySteps.contains(_))

      val (newWorkers, unstartedSteps) = distributeWork(remainingWorkers, readySteps)
      TickState(
        calculateNextTick(newWorkers),
        newWorkers.toArray,
        remainingSteps ++ unstartedSteps.map(_ -> Set.empty[String]))
    }

    private def calculateNextTick(newWorkers: Seq[Option[Work]]) = {
      newWorkers.foldLeft(Int.MaxValue) {
        case (seconds, Some(worker)) if worker.remaining < seconds => worker.remaining
        case (seconds, _) => seconds
      }
    }

    private def distributeWork(currentSlots: Array[Option[Work]], readySteps: List[String]): (Seq[Option[Work]], List[String]) = {
      currentSlots.foldLeft((Seq.empty[Option[Work]], readySteps)) {
        case ((newWorkSlots, nextStepToWork :: remainingSteps), None) =>
          (newWorkSlots :+ Some(Work(nextStepToWork, timeForStep(nextStepToWork))), remainingSteps)
        case ((newWorkSlots, steps), work) => (newWorkSlots :+ work, steps)
      }
    }

    private def finishedAndRemainingWorkers(): (Array[Option[Work]], Array[Option[Work]]) = {
      val updatedWorkers = workSlots.map(_.map(_.deductPassedSeconds(secondsInTick)))
      val finishedWorkers = updatedWorkers.filter(_.exists(_.remaining == 0))

      val remainingWorkers = updatedWorkers.map {
        case finished if finishedWorkers.contains(finished) => None
        case working => working
      }

      (finishedWorkers, remainingWorkers)
    }
  }

  case class Work(step: String, remaining: Int) {
    def deductPassedSeconds(seconds: Int): Work = copy(remaining = remaining - seconds)
  }

  private def createAllSteps(stepsWithRequirements: Map[String, Set[String]]) = {
    val allStepLetters = stepsWithRequirements.flatMap(requirement => requirement._2 + requirement._1).toSet
    val startLetters = allStepLetters.diff(stepsWithRequirements.keys.toSet)
    val allSteps = stepsWithRequirements ++ startLetters.map(s => s -> Set.empty[String])
    allSteps
  }

  private def readSteps() = {
    readLines("day7.txt")
      .map {
        case lineRegex(finish, begin) =>
          (finish, begin)
      }
      .toList
      .groupBy(_._2)
      .mapValues(_.map(_._1).toSet)
  }

}
