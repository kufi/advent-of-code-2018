package ch.kufi.aoc

class Day16 extends Challenge[Int, Int] {

  private val stateRegex = """.*\[([0-9]*), ([0-9]*), ([0-9]*), ([0-9]*)\]""".r
  private val opCodeRegex = """([0-9]*) ([0-9]*) ([0-9]*) ([0-9]*)""".r

  private val opcodeInitializers = List[(Int, Int, Int) => Opcode](
    Addr.fromInts,
    Addi.fromInts,
    Mulr.fromInts,
    Muli.fromInts,
    Banr.fromInts,
    Bani.fromInts,
    Borr.fromInts,
    Bori.fromInts,
    Setr.fromInts,
    Seti.fromInts,
    Gtir.fromInts,
    Gtri.fromInts,
    Gtrr.fromInts,
    Eqir.fromInts,
    Eqri.fromInts,
    Eqrr.fromInts
  )

  override def part1(): Int = {
    readLines("day16-part1.txt")
      .grouped(4)
      .map {
        case stateRegex(in0, in1, in2, in3) :: opCodeRegex(number, a, b, c) :: stateRegex(out0, out1, out2, out3) :: unused =>
          val startState = DeviceState(Vector(in0.toInt, in1.toInt, in2.toInt, in3.toInt))
          val endState = DeviceState(Vector(out0.toInt, out1.toInt, out2.toInt, out3.toInt))
          opcodeInitializers.count(_.apply(a.toInt, b.toInt, c.toInt).run(startState) == endState)
      }
      .count(_ >= 3)
  }

  override def part2(): Int = {
    val matchingOpcodes = readLines("day16-part1.txt")
      .grouped(4)
      .map {
        case stateRegex(in0, in1, in2, in3) :: opCodeRegex(number, a, b, c) :: stateRegex(out0, out1, out2, out3) :: unused =>
          val startState = DeviceState(Vector(in0.toInt, in1.toInt, in2.toInt, in3.toInt))
          val endState = DeviceState(Vector(out0.toInt, out1.toInt, out2.toInt, out3.toInt))
          number.toInt -> opcodeInitializers.filter(_.apply(a.toInt, b.toInt, c.toInt).run(startState) == endState)
      }
      .toList
      .groupBy(_._1)
      .mapValues(reduceToCompleteMatches)

    val finalOpcodes = deduplicateOpcodes(matchingOpcodes)

    val finalState = readLines("day16-programm.txt").foldLeft(DeviceState(Vector(0, 0, 0, 0))) {
      case (state, opCodeRegex(number, a, b, c)) =>
        finalOpcodes(number.toInt).apply(a.toInt, b.toInt, c.toInt).run(state)
    }

    finalState.read(Register(0)).value
  }

  private def reduceToCompleteMatches(allOpcodes: List[(Int, List[(Int, Int, Int) => Opcode])]): Set[(Int, Int, Int) => Opcode] = {
    allOpcodes
      .map(_._2.toSet)
      .reduce((matchedOpcodes, newOpcodes) => matchedOpcodes.intersect(newOpcodes))
  }

  private def deduplicateOpcodes(matchingOpcodes: Map[Int, Set[(Int, Int, Int) => Opcode]]): Map[Int, (Int, Int, Int) => Opcode] = {
    Iterator
      .iterate(matchingOpcodes) { allOpcodes =>
        val finalOpcodes = allOpcodes.filter(_._2.size == 1)
        val nonFinalOpcodes = allOpcodes.filter(_._2.size > 1)
        nonFinalOpcodes.mapValues(_ -- finalOpcodes.values.flatten) ++ finalOpcodes
      }
      .find(_.values.forall(opcodes => opcodes.size == 1))
      .get
      .mapValues(_.head)
  }
}

case class DeviceState(registers: Vector[Int]) {
  def read(register: Register): Value = Value(registers(register.number))

  def write(register: Register, value: Value): DeviceState = DeviceState(registers.updated(register.number, value.value))
}

case class Register(number: Int)

case class Value(value: Int) extends Ordered[Value] {

  def +(other: Value) = Value(value + other.value)

  def *(other: Value) = Value(value * other.value)

  def &(other: Value) = Value(value & other.value)

  def |(other: Value) = Value(value | other.value)

  override def compare(that: Value): Int = value - that.value
}

sealed trait Opcode {
  def run(state: DeviceState): DeviceState
}

case class Addr(a: Register, b: Register, c: Register) extends Opcode {
  override def run(state: DeviceState): DeviceState = state.write(c, state.read(a) + state.read(b))
}

case object Addr {
  def fromInts(a: Int, b: Int, c: Int) = Addr(Register(a), Register(b), Register(c))
}

case class Addi(a: Register, b: Value, c: Register) extends Opcode {
  override def run(state: DeviceState): DeviceState = state.write(c, state.read(a) + b)
}

case object Addi {
  def fromInts(a: Int, b: Int, c: Int) = Addi(Register(a), Value(b), Register(c))
}

case class Mulr(a: Register, b: Register, c: Register) extends Opcode {
  override def run(state: DeviceState): DeviceState = state.write(c, state.read(a) * state.read(b))
}

case object Mulr {
  def fromInts(a: Int, b: Int, c: Int) = Mulr(Register(a), Register(b), Register(c))
}

case class Muli(a: Register, b: Value, c: Register) extends Opcode {
  override def run(state: DeviceState): DeviceState = state.write(c, state.read(a) * b)
}

case object Muli {
  def fromInts(a: Int, b: Int, c: Int) = Muli(Register(a), Value(b), Register(c))
}

case class Banr(a: Register, b: Register, c: Register) extends Opcode {
  override def run(state: DeviceState): DeviceState = state.write(c, state.read(a) & state.read(b))
}

case object Banr {
  def fromInts(a: Int, b: Int, c: Int) = Banr(Register(a), Register(b), Register(c))
}

case class Bani(a: Register, b: Value, c: Register) extends Opcode {
  override def run(state: DeviceState): DeviceState = state.write(c, state.read(a) & b)
}

case object Bani {
  def fromInts(a: Int, b: Int, c: Int) = Bani(Register(a), Value(b), Register(c))
}

case class Borr(a: Register, b: Register, c: Register) extends Opcode {
  override def run(state: DeviceState): DeviceState = state.write(c, state.read(a) | state.read(b))
}

case object Borr {
  def fromInts(a: Int, b: Int, c: Int) = Borr(Register(a), Register(b), Register(c))
}

case class Bori(a: Register, b: Value, c: Register) extends Opcode {
  override def run(state: DeviceState): DeviceState = state.write(c, state.read(a) | b)
}

case object Bori {
  def fromInts(a: Int, b: Int, c: Int) = Bori(Register(a), Value(b), Register(c))
}

case class Setr(a: Register, b: Register, c: Register) extends Opcode {
  override def run(state: DeviceState): DeviceState = state.write(c, state.read(a))
}

case object Setr {
  def fromInts(a: Int, b: Int, c: Int) = Setr(Register(a), Register(b), Register(c))
}

case class Seti(a: Value, b: Register, c: Register) extends Opcode {
  override def run(state: DeviceState): DeviceState = state.write(c, a)
}

case object Seti {
  def fromInts(a: Int, b: Int, c: Int) = Seti(Value(a), Register(b), Register(c))
}

case class Gtir(a: Value, b: Register, c: Register) extends Opcode {
  override def run(state: DeviceState): DeviceState = {
    if (a > state.read(b)) {
      state.write(c, Value(1))
    } else {
      state.write(c, Value(0))
    }
  }
}

case object Gtir {
  def fromInts(a: Int, b: Int, c: Int) = Gtir(Value(a), Register(b), Register(c))
}

case class Gtri(a: Register, b: Value, c: Register) extends Opcode {
  override def run(state: DeviceState): DeviceState = {
    if (state.read(a) > b) {
      state.write(c, Value(1))
    } else {
      state.write(c, Value(0))
    }
  }
}

case object Gtri {
  def fromInts(a: Int, b: Int, c: Int) = Gtri(Register(a), Value(b), Register(c))
}

case class Gtrr(a: Register, b: Register, c: Register) extends Opcode {
  override def run(state: DeviceState): DeviceState = {
    if (state.read(a) > state.read(b)) {
      state.write(c, Value(1))
    } else {
      state.write(c, Value(0))
    }
  }
}

case object Gtrr {
  def fromInts(a: Int, b: Int, c: Int) = Gtrr(Register(a), Register(b), Register(c))
}

case class Eqir(a: Value, b: Register, c: Register) extends Opcode {
  override def run(state: DeviceState): DeviceState = {
    if (a == state.read(b)) {
      state.write(c, Value(1))
    } else {
      state.write(c, Value(0))
    }
  }
}

case object Eqir {
  def fromInts(a: Int, b: Int, c: Int) = Eqir(Value(a), Register(b), Register(c))
}

case class Eqri(a: Register, b: Value, c: Register) extends Opcode {
  override def run(state: DeviceState): DeviceState = {
    if (state.read(a) == b) {
      state.write(c, Value(1))
    } else {
      state.write(c, Value(0))
    }
  }
}

case object Eqri {
  def fromInts(a: Int, b: Int, c: Int) = Eqri(Register(a), Value(b), Register(c))
}

case class Eqrr(a: Register, b: Register, c: Register) extends Opcode {
  override def run(state: DeviceState): DeviceState = {
    if (state.read(a) == state.read(b)) {
      state.write(c, Value(1))
    } else {
      state.write(c, Value(0))
    }
  }
}

case object Eqrr {
  def fromInts(a: Int, b: Int, c: Int) = Eqrr(Register(a), Register(b), Register(c))
}
