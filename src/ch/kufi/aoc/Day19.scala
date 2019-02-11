package ch.kufi.aoc

class Day19 extends Challenge[Int, Int] {
  private val opCodeRegex = """([a-z]*) ([0-9]*) ([0-9]*) ([0-9]*)""".r

  private val opCodeInitializers = Map[String, (Int, Int, Int) => Opcode](
    "addr" -> Addr.fromInts,
    "addi" -> Addi.fromInts,
    "mulr" -> Mulr.fromInts,
    "muli" -> Muli.fromInts,
    "banr" -> Banr.fromInts,
    "bani" -> Bani.fromInts,
    "borr" -> Borr.fromInts,
    "bori" -> Bori.fromInts,
    "setr" -> Setr.fromInts,
    "seti" -> Seti.fromInts,
    "gtir" -> Gtir.fromInts,
    "gtri" -> Gtri.fromInts,
    "gtrr" -> Gtrr.fromInts,
    "eqir" -> Eqir.fromInts,
    "eqri" -> Eqri.fromInts,
    "eqrr" -> Eqrr.fromInts
  )

  override def part1(): Int = {
    val (ipRegister, programm) = readProgram(readLines("day19.txt").toList)
    val endState = runProgramm(DeviceState(Vector.fill(6)(0)), programm, ipRegister)

    endState.read(Register(0)).value
  }

  override def part2(): Int = {
    //the programm sums all divisors of a given number. Instead of doing it the hard way, get the number
    //and calculate it directly in Scala
    val (ipRegister, programm) = readProgram(readLines("day19.txt").toList)
    val updatedProgram = programm.updated(1, Addi(Register(2), Value(200), Register(2)))

    val endState = runProgramm(DeviceState(Vector(1, 0, 0, 0, 0, 0)), updatedProgram, ipRegister)

    val number = endState.read(Register(3)).value

    findDivisors(number).sum
  }

  def findDivisors(number: Int): Set[Int] = {
    Stream.from(1)
      .take(Math.sqrt(number).toInt)
      .toSet
      .filter(n => number % n == 0)
      .flatMap(n => Set(n, number/n))
  }


  private def readProgram(input: List[String]) = {
    val ipRegister = Register(input.head.replace("#ip ", "").toInt)
    val programm = input.tail.map {
      case opCodeRegex(opCode, in1, in2, in3) =>
        opCodeInitializers(opCode)(in1.toInt, in2.toInt, in3.toInt)
    }
    (ipRegister, programm)
  }

  private def runProgramm(initialState: DeviceState, programm: List[Opcode], ipRegister: Register) = {
    Iterator
      .iterate(initialState) { deviceState =>
        val newState = programm(deviceState.read(ipRegister).value).run(deviceState)
        newState.write(ipRegister, newState.read(ipRegister) + Value(1))
      }
      .find(_.read(ipRegister).value >= programm.size)
      .get
  }
}
