package dayXX

import helpers.*
import zio.*
import zio.test.*
import zio.test.Assertion.*
import zio.test.TestAspect.{failing, ignore, timeout}

import scala.annotation.tailrec
import scala.math.*
import scala.util.chaining.*

// ------------------------------------------------------------------------------
def parse(input: String) =
  input
    .split("\n")

// ------------------------------------------------------------------------------

def resolveStar1(input: String): BigInt =
  0

// ------------------------------------------------------------------------------

def resolveStar2(input: String): BigInt =
  0

// ------------------------------------------------------------------------------

object PuzzleXXTest extends ZIOSpecDefault {
  val day  = "dayXX"
  def spec = suite(s"puzzle $day")(
    test("star#1") {
      for {
        exampleInput <- Helpers.readFileContent(s"data/$day/example-1.txt")
        exampleResult = resolveStar1(exampleInput)
        puzzleInput  <- Helpers.readFileContent(s"data/$day/puzzle-1.txt")
        puzzleResult  = resolveStar1(puzzleInput)
      } yield assertTrue(
        exampleResult == BigInt(0),
        puzzleResult == BigInt(0)
      )
    },
    test("star#2") {
      for {
        exampleInput1 <- Helpers.readFileContent(s"data/$day-example-1.txt")
        exampleResult1 = resolveStar2(exampleInput1)
        puzzleInput   <- Helpers.readFileContent(s"data/$day-puzzle-1.txt")
        puzzleResult   = resolveStar2(puzzleInput)
      } yield assertTrue(
        exampleResult1 == BigInt(0),
        puzzleResult == BigInt(0)
      )
    }
  )
}
