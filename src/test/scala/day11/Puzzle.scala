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
  factorToTest: Int,
  onTrueSendTo: Int,
  onFalseSendTo: Int,
  inspected: Long = 0
) {
  override def toString: String = s"Monkey $id: ${items.mkString(", ")}"
}

def parseItems(input: String): List[Long] =
  input.split(": ", 2).toList.drop(1).head.split(", ").map(_.toLong).toList

val operationRE = """Operation: new = old ([+*]) (.+)""".r

def parseOperation(input: String): Long => Long = {
  input.trim match {
    case operationRE("+", "old")  => x => x + x
    case operationRE("*", "old")  => x => x * x
    case operationRE("+", number) => x => x + number.toInt
    case operationRE("*", number) => x => x * number.toInt
  }
}

val testRE = """Test: divisible by (\d+)""".r

def parseTest(input: String): Int = {
  input.trim match {
    case testRE(number) => number.toInt
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

def round(monkeys: Chunk[Monkey], withSuperModulo: Boolean = false): Chunk[Monkey] = {
  lazy val superModulo = monkeys.map(_.factorToTest).product

  val simplifier: Long => Long =
    if (withSuperModulo) (x: Long) => x % superModulo else (x: Long) => x / 3

  @annotation.tailrec
  def worker(currentMonkeyId: Int, state: Chunk[Monkey]): Chunk[Monkey] = {
    state.lift(currentMonkeyId) match {
      case None                                 => state
      case Some(monkey) if monkey.items.isEmpty => worker(currentMonkeyId + 1, state)
      case Some(monkey)                         =>
        val (item, updatedQueue) = monkey.items.dequeue
        val worryLevel           = simplifier(monkey.operation(item))
        val sendToMoneyId        = if (worryLevel % monkey.factorToTest == 0) monkey.onTrueSendTo else monkey.onFalseSendTo
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
def rounds(howMany: Int, monkeys: Chunk[Monkey], withSuperModulo: Boolean = false): Chunk[Monkey] =
  if (howMany > 0) rounds(howMany - 1, round(monkeys, withSuperModulo), withSuperModulo) else monkeys

// ==============================================================================

def resolveStar1(input: List[String]): Long =
  val monkeys = parse(input)
  val result  = rounds(20, monkeys)
  result.sortBy(-_.inspected).map(_.inspected).take(2).product

// ==============================================================================

def resolveStar2(input: List[String]): Long =
  val monkeys = parse(input)
  val result  = rounds(10000, monkeys, true)
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
