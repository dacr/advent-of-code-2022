package day03

import zio.*
import zio.test.*
import zio.nio.file.Path
import helpers.Helpers.*
import zio.test.TestAspect.*

// ------------------------------------------------------------------------------
case class Item(code: Char) extends AnyVal {
  def priority =
    if (code.isLower) code - 'a' + 1
    else code - 'A' + 27
}

case class Rucksack(first: Set[Item], second: Set[Item]) {
  val all = first ++ second
}

def parse(input: List[String]): List[Rucksack] =
  input
    .map { line =>
      line.map(Item.apply).splitAt(line.size / 2) match {
        case (first, second) => Rucksack(first.toSet, second.toSet)
      }
    }

def resolveStar1(input: List[String]): Int =
  parse(input)
    .flatMap(rucksack => rucksack.first.intersect(rucksack.second).map(_.priority).headOption)
    .sum

// ------------------------------------------------------------------------------

def resolveStar2(input: List[String]): Int = {
  parse(input)
    .grouped(3)
    .flatMap(rucksacks => rucksacks.map(_.all).reduce(_ intersect _).map(_.priority).headOption)
    .sum
}
// ------------------------------------------------------------------------------

object Puzzle03Test extends ZIOSpecDefault {
  val day  = getClass.getName.replaceAll(""".*Puzzle(\d+)Test.*""", "day$1")
  def spec = suite(s"puzzle $day")(
    test("star#1") {
      for {
        exampleInput <- fileLines(Path(s"data/$day/example-1.txt"))
        exampleResult = resolveStar1(exampleInput)
        puzzleInput  <- fileLines(Path(s"data/$day/puzzle-1.txt"))
        puzzleResult  = resolveStar1(puzzleInput)
      } yield assertTrue(
        exampleResult == 157,
        puzzleResult == 7428
      )
    },
    test("star#2") {
      for {
        exampleInput <- fileLines(Path(s"data/$day/example-1.txt"))
        exampleResult = resolveStar2(exampleInput)
        puzzleInput  <- fileLines(Path(s"data/$day/puzzle-1.txt"))
        puzzleResult  = resolveStar2(puzzleInput)
      } yield assertTrue(
        exampleResult == 70,
        puzzleResult == 2650
      )
    }
  ) @@ timed @@ sequential
}
