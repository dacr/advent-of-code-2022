package day04

import zio.*
import zio.test.*
import zio.nio.file.Path
import helpers.Helpers.*

// ------------------------------------------------------------------------------

def resolveStar1(input: List[String]): Int =
  0

// ------------------------------------------------------------------------------

def resolveStar2(input: List[String]): Int =
  0

// ------------------------------------------------------------------------------

object Puzzle04Test extends ZIOSpecDefault {
  val day  = "day04"
  def spec = suite(s"puzzle $day")(
    test("star#1") {
      for {
        exampleInput <- fileLines(Path(s"data/$day/example-1.txt"))
        exampleResult = resolveStar1(exampleInput)
        puzzleInput  <- fileLines(Path(s"data/$day/puzzle-1.txt"))
        puzzleResult  = resolveStar1(puzzleInput)
      } yield assertTrue(
        exampleResult == 0,
        puzzleResult == 0
      )
    },
    test("star#2") {
      for {
        exampleInput <- fileLines(Path(s"data/$day/example-1.txt"))
        exampleResult = resolveStar2(exampleInput)
        puzzleInput  <- fileLines(Path(s"data/$day/puzzle-1.txt"))
        puzzleResult  = resolveStar2(puzzleInput)
      } yield assertTrue(
        exampleResult == 0,
        puzzleResult == 0
      )
    }
  )
}
