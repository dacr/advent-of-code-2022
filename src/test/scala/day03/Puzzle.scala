package day03

import helpers.*
import zio.*
import zio.test.*
import zio.test.Assertion.*

// ------------------------------------------------------------------------------
case class Item(code: Char) extends AnyVal {
  def priority =
    if (code >= 'a' && code <= 'z') code - 'a' + 1
    else code - 'A' + 27
}

case class Rucksack(first: Set[Item], second: Set[Item]) {
  val all = first ++ second
}

def parse(input: String): List[Rucksack] =
  input
    .split("\n")
    .toList
    .map { line =>
      line.map(Item.apply).splitAt(line.size / 2) match {
        case (first, second) => Rucksack(first.toSet, second.toSet)
      }
    }

def resolveStar1(input: String): Int =
  parse(input).map { rucksack =>
    rucksack.first.intersect(rucksack.second).map(_.priority).sum
  }.sum

// ------------------------------------------------------------------------------

def resolveStar2(input: String): Int = {
  parse(input)
    .grouped(3)
    .map(rucksacks => rucksacks.map(_.all).reduce(_ intersect _).map(_.priority).sum)
    .sum
}
// ------------------------------------------------------------------------------

object Puzzle03Test extends ZIOSpecDefault {
  val day  = "day03"
  def spec = suite(s"puzzle $day")(
    test("star#1") {
      for {
        exampleInput <- Helpers.readFileContent(s"data/$day/example-1.txt")
        exampleResult = resolveStar1(exampleInput)
        puzzleInput  <- Helpers.readFileContent(s"data/$day/puzzle-1.txt")
        puzzleResult  = resolveStar1(puzzleInput)
      } yield assertTrue(
        exampleResult == 157,
        puzzleResult == 7428
      )
    },
    test("star#2") {
      for {
        exampleInput <- Helpers.readFileContent(s"data/$day/example-1.txt")
        exampleResult = resolveStar2(exampleInput)
        puzzleInput  <- Helpers.readFileContent(s"data/$day/puzzle-1.txt")
        puzzleResult  = resolveStar2(puzzleInput)
      } yield assertTrue(
        exampleResult == 70,
        puzzleResult == 2650
      )
    }
  )
}
