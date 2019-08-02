package ch.kufi.aoc

class Day21 extends Challenge[Int, Int] {
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
    val (ipRegister, programm) = readProgram(readLines("day21.txt").toList)
    val endState = runProgramm(DeviceState(Vector.fill(6)(0)), programm, ipRegister)

    endState.find(a => a.registers(2) == 28).get.registers(5)
  }

  override def part2(): Int = {
    val (ipRegister, programm) = readProgram(readLines("day21.txt").toList)
    val endState = runProgramm(DeviceState(Vector.fill(6)(0)), programm, ipRegister)

    var i = 0

    endState
      .filter(a => a.registers(2) == 28)
      .map(a => a.registers(5))
      .scanLeft((Set.empty[Int], List.empty[Int], 0))((a, b) => (a._2.toSet, a._2 :+ b, b))
      .find(a => a._1.contains(a._3))
      .get
      ._2
      .init
      .last
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
  }
}
