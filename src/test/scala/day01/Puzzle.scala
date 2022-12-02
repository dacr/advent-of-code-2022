package day01

import helpers.*
import zio.*
import zio.test.*
import zio.test.Assertion.*

// ------------------------------------------------------------------------------

def resolveStar1(input: String): Int = {
  input.trim
    .split("\n\n")
    .toList
    .map(_.split("\n").map(_.toInt).sum)
    .max
}
// ------------------------------------------------------------------------------

def resolveStar2(input: String): Int = {
  input.trim
    .split("\n\n")
    .toList
    .map(_.split("\n").map(_.toInt).sum)
    .sortBy(x => -x)
    .take(3)
    .sum
}
// ------------------------------------------------------------------------------

object PuzzleTest extends ZIOSpecDefault {
  val day  = "day01"
  def spec = suite(s"puzzle $day")(
    test("star#1") {
      for {
        exampleInput <- Helpers.readFileContent(s"data/$day/example-1.txt")
        exampleResult = resolveStar1(exampleInput)
        puzzleInput  <- Helpers.readFileContent(s"data/$day/puzzle-1.txt")
        puzzleResult  = resolveStar1(puzzleInput)
      } yield assertTrue(
        exampleResult == 24000,
        puzzleResult == 66616
      )
    },
    test("star#2") {
      for {
        exampleInput <- Helpers.readFileContent(s"data/$day/example-1.txt")
        exampleResult = resolveStar2(exampleInput)
        puzzleInput  <- Helpers.readFileContent(s"data/$day/puzzle-1.txt")
        puzzleResult  = resolveStar2(puzzleInput)
      } yield assertTrue(
        exampleResult == 45000,
        puzzleResult == 199172
      )
    }
  )
}
