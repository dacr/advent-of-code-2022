package day05

import zio.*
import zio.test.*
import scala.util.chaining._
import zio.test.TestAspect.*

// ------------------------------------------------------------------------------
case class Crate(mark: String) extends AnyVal {
  override def toString: String = mark
}

case class Instruction(howMany: Int, fromIndex: Int, toIndex: Int)

// ------------------------------------------------------------------------------
def parse(lines: List[String]): (Chunk[List[Crate]], List[Instruction]) = {
  lines.span(_ != "") match {
    case (stacksLines, instructionsLines) =>
      (parseStacks(stacksLines), parseInstructions(instructionsLines))
  }
}

def parseInstructions(lines: List[String]): List[Instruction] = {
  val instructionRE = """move (\d+) from (\d+) to (\d+)""".r
  lines.collect { case instructionRE(howMany, from, to) =>
    Instruction(howMany.toInt, from.toInt - 1, to.toInt - 1)
  }
}

def parseStacks(lines: List[String]): Chunk[List[Crate]] = {
  val stacksSize  = lines.last.trim.split("""\s+""").size
  val emptyStacks = Chunk.fill(stacksSize)(List.empty[Crate])
  lines.init.foldLeft(emptyStacks) { (stacks, line) =>
    // "[Z] [M]     [P]" => "ZM P"
    val emptyMark     = " "
    val convertedLine = line.grouped(4).map { rawMark =>
      rawMark
        .replaceAll("""\[(.+)\]\s*""", "$1")
        .replaceAll("""\s+""", emptyMark)
    }
    val updatedStacks = convertedLine.zipWithIndex.foldLeft(stacks) { case (updatingStacks, (mark, stackIndex)) =>
      if (mark == emptyMark) updatingStacks
      else updatingStacks.updated(stackIndex, updatingStacks(stackIndex) :+ Crate(mark))
    }
    updatedStacks
  }
}

// ------------------------------------------------------------------------------

def resolveStar1(lines: List[String]): String = {
  val (stacks, instructions) = parse(lines)
  val updatedStacks          = instructions.foldLeft(stacks) { (updatingStacks, instruction) =>
    val theFromStack   = updatingStacks(instruction.fromIndex)
    val theToStack     = updatingStacks(instruction.toIndex)
    val selectedCrates = theFromStack.take(instruction.howMany).reverse
    updatingStacks
      .updated(instruction.fromIndex, theFromStack.drop(instruction.howMany))
      .updated(instruction.toIndex, selectedCrates ::: theToStack)
  }
  updatedStacks.map(_.headOption.getOrElse(" ")).mkString
}

// ------------------------------------------------------------------------------

def resolveStar2(lines: List[String]): String = {
  val (stacks, instructions) = parse(lines)
  val updatedStacks          = instructions.foldLeft(stacks) { (updatingStacks, instruction) =>
    val theFromStack   = updatingStacks(instruction.fromIndex)
    val theToStack     = updatingStacks(instruction.toIndex)
    val selectedCrates = theFromStack.take(instruction.howMany) // THE SAME AS PREVIOUS BUT WITHOUT REVERSE
    updatingStacks
      .updated(instruction.fromIndex, theFromStack.drop(instruction.howMany))
      .updated(instruction.toIndex, selectedCrates ::: theToStack)
  }
  updatedStacks.map(_.headOption.getOrElse(" ")).mkString
}

// ------------------------------------------------------------------------------

object Puzzle05Test extends ZIOSpecDefault {
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
        exampleResult == "CMZ",
        puzzleResult == "FJSRQCFTN"
      )
    },
    test("star#2") {
      for {
        exampleInput <- fileLines(Path(s"data/$day/example-1.txt"))
        exampleResult = resolveStar2(exampleInput)
        puzzleInput  <- fileLines(Path(s"data/$day/puzzle-1.txt"))
        puzzleResult  = resolveStar2(puzzleInput)
      } yield assertTrue(
        exampleResult == "MCD",
        puzzleResult == "CJVLJQPHS"
      )
    }
  ) @@ timed @@ sequential
}
