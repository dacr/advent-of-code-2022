package day05

import zio.*
import zio.test.*
import scala.util.chaining._

// ------------------------------------------------------------------------------
def parse(input: List[String]) =
  input

// ------------------------------------------------------------------------------

def resolveStar1(lines: List[String]): Int =
  0

// ------------------------------------------------------------------------------

def resolveStar2(lines: List[String]): Int =
  0

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
