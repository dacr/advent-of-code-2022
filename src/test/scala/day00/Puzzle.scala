package day00

import zio.*
import zio.test.*


// ------------------------------------------------------------------------------
def parse(input: List[String]) =
  input

// ------------------------------------------------------------------------------

def resolveStar1(input: List[String]): BigInt =
  0

// ------------------------------------------------------------------------------

def resolveStar2(input: List[String]): BigInt =
  0

// ------------------------------------------------------------------------------

object Puzzle00Test extends ZIOSpecDefault {
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
        exampleResult == BigInt(0),
        puzzleResult == BigInt(0)
      )
    },
    test("star#2") {
      for {
        exampleInput1 <- fileLines(Path(s"data/$day/example-1.txt"))
        exampleResult1 = resolveStar2(exampleInput1)
        puzzleInput   <- fileLines(Path(s"data/$day/puzzle-1.txt"))
        puzzleResult   = resolveStar2(puzzleInput)
      } yield assertTrue(
        exampleResult1 == BigInt(0),
        puzzleResult == BigInt(0)
      )
    }
  )
}