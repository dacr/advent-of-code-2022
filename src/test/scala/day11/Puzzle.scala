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

def resolveStar1(input: List[String]): Int =
  val monkeys = parse(input)
  val result  = rounds(20, monkeys)
  result.sortBy(-_.inspected).map(_.inspected).take(2).product

// ------------------------------------------------------------------------------

def resolveStar2(input: List[String]): BigInt =
  val monkeys = parse(input)
  val result  = rounds(10000, monkeys)
  result.sortBy(-_.inspected).map(_.inspected).take(2).product

// ------------------------------------------------------------------------------

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
        exampleResult == 10605,
        puzzleResult == 61005
      )
    },
    test("star#2") {
      for {
        exampleInput1 <- fileLines(Path(s"data/$day/example-1.txt"))
        exampleResult1 = resolveStar2(exampleInput1)
        puzzleInput   <- fileLines(Path(s"data/$day/puzzle-1.txt"))
        puzzleResult   = resolveStar2(puzzleInput)
      } yield assertTrue(
        exampleResult1 == BigInt("2713310158"),
        puzzleResult == BigInt("0")
      )
    }
  ) @@ timed @@ sequential
}
