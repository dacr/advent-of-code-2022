package day06

import zio.*
import zio.test.*
import zio.test.TestAspect.*

def findMarker(input: String, ofLength: Int): Int =
  input
    .sliding(ofLength)
    .zipWithIndex
    .collect { case (group, index) if group.distinct.size == ofLength => index + ofLength }
    .next()

// ------------------------------------------------------------------------------

def resolveStar1(input: String): Int = findMarker(input, 4)

// ------------------------------------------------------------------------------

def resolveStar2(input: String): Int = findMarker(input, 14)

// ------------------------------------------------------------------------------

object Puzzle06Test extends ZIOSpecDefault {
  import zio.nio.file.Path
  import helpers.Helpers.*
  val day  = getClass.getName.replaceAll(""".*Puzzle(\d+)Test.*""", "day$1")
  def spec = suite(s"puzzle $day")(
    test("star#1") {
      for {
        exampleInput <- fileContent(Path(s"data/$day/example-1.txt"))
        exampleResult = resolveStar1(exampleInput)
        puzzleInput  <- fileContent(Path(s"data/$day/puzzle-1.txt"))
        puzzleResult  = resolveStar1(puzzleInput)
      } yield assertTrue(
        exampleResult == 7,
        resolveStar1("bvwbjplbgvbhsrlpgdmjqwftvncz") == 5,
        resolveStar1("nppdvjthqldpwncqszvftbrmjlhg") == 6,
        resolveStar1("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg") == 10,
        resolveStar1("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw") == 11,
        puzzleResult == 1287
      )
    },
    test("star#2") {
      for {
        exampleInput1 <- fileContent(Path(s"data/$day/example-1.txt"))
        exampleResult1 = resolveStar2(exampleInput1)
        puzzleInput   <- fileContent(Path(s"data/$day/puzzle-1.txt"))
        puzzleResult   = resolveStar2(puzzleInput)
      } yield assertTrue(
        exampleResult1 == 19,
        resolveStar2("bvwbjplbgvbhsrlpgdmjqwftvncz") == 23,
        resolveStar2("nppdvjthqldpwncqszvftbrmjlhg") == 23,
        resolveStar2("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg") == 29,
        resolveStar2("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw") == 26,
        puzzleResult == 3716
      )
    }
  ) @@ timed @@ sequential
}
