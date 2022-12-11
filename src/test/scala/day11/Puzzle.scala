package day11

import zio.*
import zio.test.*
import zio.test.TestAspect.*
import scala.collection.immutable.{Queue => SQueue}

// ------------------------------------------------------------------------------
case class Monkey(
  id: Int,
  items: SQueue[Long],
  operation: Long => Long,
  test: Long => Boolean,
  onTrueSendTo: Int,
  onFalseSendTo: Int,
  inspected: Int = 0
) {
  override def toString: String = s"Monker $id: ${items.mkString(", ")}"
}

// ------------------------------------------------------------------------------
def parseItems(input: String): List[Long] =
  input.split(": ", 2).toList.drop(1).head.split(", ").map(_.toLong).toList

val operationRE = """Operation: new = old ([+*]) (.+)""".r

def parseOperation(input: String): Long => Long = {
  input.trim match {
    case operationRE("+", "old")  => x => x + x
    case operationRE("*", "old")  => x => x * x
    case operationRE("+", number) => x => x + number.toLong
    case operationRE("*", number) => x => x * number.toLong
  }
}

val testRE = """Test: divisible by (\d+)""".r

def parseTest(input: String): Long => Boolean = {
  input.trim match {
    case testRE(number) => x => x % number.toLong == 0
  }
}

def parse(input: List[String]): Chunk[Monkey] = {
  val monkeys =
    input
      .grouped(7)
      .zipWithIndex
      .map { case (lines: List[String], index) =>
        val id        = index
        val items     = parseItems(lines.drop(1).head)
        val operation = parseOperation(lines.drop(2).head)
        val test      = parseTest(lines.drop(3).head)
        val onTrue    = lines.drop(4).head.trim.split(" ").last.toInt
        val onFalse   = lines.drop(5).head.trim.split(" ").last.toInt
        Monkey(id, SQueue.from(items), operation, test, onTrue, onFalse)
      }
  Chunk.from(monkeys)
}

// ------------------------------------------------------------------------------

def round(monkeys: Chunk[Monkey]): Chunk[Monkey] = {
  @annotation.tailrec
  def worker(currentMonkeyId: Int, state: Chunk[Monkey]): Chunk[Monkey] = {
    state.lift(currentMonkeyId) match {
      case None                                 => state
      case Some(monkey) if monkey.items.isEmpty => worker(currentMonkeyId + 1, state)
      case Some(monkey)                         =>
        val (item, updatedQueue) = monkey.items.dequeue
        val worryLevel           = monkey.operation(item) / 3
        val sendToMoneyId        = if (monkey.test(worryLevel)) monkey.onTrueSendTo else monkey.onFalseSendTo
        val target               = state(sendToMoneyId)
        val updatedInspected     = monkey.inspected + 1
        val updatedState         =
          state
            .updated(currentMonkeyId, monkey.copy(items = updatedQueue, inspected = updatedInspected))
            .updated(sendToMoneyId, target.copy(items = target.items.enqueue(worryLevel)))
        worker(currentMonkeyId, updatedState)
    }
  }
  worker(0, monkeys)
}

def rounds(howMany: Int, monkeys: Chunk[Monkey]): Chunk[Monkey] =
  if (howMany > 0) rounds(howMany - 1, round(monkeys)) else monkeys

// ------------------------------------------------------------------------------

def resolveStar1(input: List[String]): Long =
  val monkeys = parse(input)
  val result  = rounds(20, monkeys)
  result.sortBy(-_.inspected).map(_.inspected).take(2).product

// ==============================================================================

case class Monkey2(
  id: Int,
  items: SQueue[Long],
  operation: Long => Long,
  factorToTest: Int,
  onTrueSendTo: Int,
  onFalseSendTo: Int,
  inspected: Long = 0
) {
  override def toString: String = s"Monkey $id: ${items.mkString(", ")}"
}

def parseItems2(input: String): List[Long] =
  input.split(": ", 2).toList.drop(1).head.split(", ").map(_.toLong).toList

def parseOperation2(input: String): Long => Long = {
  input.trim match {
    case operationRE("+", "old")  => x => x + x
    case operationRE("*", "old")  => x => x * x
    case operationRE("+", number) => x => x + number.toInt
    case operationRE("*", number) => x => x * number.toInt
  }
}

def parseTest2(input: String): Int = {
  input.trim match {
    case testRE(number) => number.toInt
  }
}

def parse2(input: List[String]): Chunk[Monkey2] = {
  val monkeys =
    input
      .grouped(7)
      .zipWithIndex
      .map { case (lines: List[String], index) =>
        val id        = index
        val items     = parseItems2(lines.drop(1).head)
        val operation = parseOperation2(lines.drop(2).head)
        val test      = parseTest2(lines.drop(3).head)
        val onTrue    = lines.drop(4).head.trim.split(" ").last.toInt
        val onFalse   = lines.drop(5).head.trim.split(" ").last.toInt
        Monkey2(id, SQueue.from(items), operation, test, onTrue, onFalse)
      }
  Chunk.from(monkeys)
}

// ------------------------------------------------------------------------------

def round2(monkeys: Chunk[Monkey2]): Chunk[Monkey2] = {
  val supermodulo = monkeys.map(_.factorToTest).product

  @annotation.tailrec
  def worker(currentMonkeyId: Int, state: Chunk[Monkey2]): Chunk[Monkey2] = {
    state.lift(currentMonkeyId) match {
      case None                                 => state
      case Some(monkey) if monkey.items.isEmpty => worker(currentMonkeyId + 1, state)
      case Some(monkey)                         =>
        val (item, updatedQueue) = monkey.items.dequeue
        val worryLevel           = monkey.operation(item) % supermodulo
        val isMultipleOf         = worryLevel             % monkey.factorToTest == 0
        val sendToMoneyId        = if (isMultipleOf) monkey.onTrueSendTo else monkey.onFalseSendTo
        val target               = state(sendToMoneyId)
        val updatedInspected     = monkey.inspected + 1

        val updatedState =
          state
            .updated(currentMonkeyId, monkey.copy(items = updatedQueue, inspected = updatedInspected))
            .updated(sendToMoneyId, target.copy(items = target.items.enqueue(worryLevel)))
        worker(currentMonkeyId, updatedState)
    }
  }
  worker(0, monkeys)
}

@annotation.tailrec
def rounds2(howMany: Int, monkeys: Chunk[Monkey2]): Chunk[Monkey2] =
  if (howMany > 0) rounds2(howMany - 1, round2(monkeys)) else monkeys

def resolveStar2(input: List[String]): Long =
  val monkeys = parse2(input)
  val result  = rounds2(10000, monkeys)
  result.sortBy(-_.inspected).map(_.inspected).take(2).product

// ==============================================================================

object Puzzle11Test extends ZIOSpecDefault {
  import zio.nio.file.Path
  import helpers.Helpers.*
  val day  = getClass.getName.replaceAll(""".*Puzzle(\d+)Test.*""", "day$1")
  def spec = suite(s"puzzle $day")(
    test("star#1") {
      for {
        exampleInput <- fileLines(Path(s"data/$day/example-1.txt"))
        exampleResult = resolveStar1(exampleInput)
        puzzleInput  <- fileLines(Path(s"data/$day/puzzle-1.txt"))
        puzzleResult  = resolveStar1(puzzleInput)
      } yield assertTrue(
        exampleResult == 10605L,
        puzzleResult == 61005L
      )
    },
    test("star#2") {
      for {
        exampleInput1 <- fileLines(Path(s"data/$day/example-1.txt"))
        exampleResult1 = resolveStar2(exampleInput1)
        puzzleInput   <- fileLines(Path(s"data/$day/puzzle-1.txt"))
        puzzleResult   = resolveStar2(puzzleInput)
      } yield assertTrue(
        exampleResult1 == 2713310158L,
        puzzleResult == 20567144694L
      )
    }
  ) @@ timed @@ sequential
}
