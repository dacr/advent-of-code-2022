package day04

import zio.*
import zio.test.*
import zio.nio.file.Path
import helpers.Helpers.*
import scala.util.chaining._
import zio.test.TestAspect.*

// ------------------------------------------------------------------------------
case class Range(from: Int, to: Int) {
  def contains(that: Range) = from <= that.from && that.to <= to
  def overlap(that: Range)  =
    (from <= that.to && that.to <= to) ||
      (from <= that.from && that.from <= to) ||
      (that.from <= to && to <= that.to) ||
      (that.from <= from && from <= that.to)
}
case class Pair(left: Range, right: Range)

val parseRE = """(\d+)-(\d+),(\d+)-(\d+)""".r

def parse(lines: List[String]): List[Pair] =
  lines.collect { case parseRE(l1, l2, r1, r2) =>
    Pair(Range(l1.toInt, l2.toInt), Range(r1.toInt, r2.toInt))
  }

// ------------------------------------------------------------------------------

def resolveStar1(lines: List[String]): Int =
  parse(lines).count { case Pair(a, b) => a.contains(b) || b.contains(a) }

// ------------------------------------------------------------------------------

def resolveStar2(lines: List[String]): Int =
  parse(lines).count { case Pair(a, b) => a.overlap(b) }

// ------------------------------------------------------------------------------

object Puzzle04Test extends ZIOSpecDefault {
  val day  = getClass.getName.replaceAll(""".*Puzzle(\d+)Test.*""", "day$1")
  def spec = suite(s"puzzle $day")(
    test("star#1") {
      for {
        exampleInput <- fileLines(Path("data/$day/example-1.txt"))
        exampleResult = resolveStar1(exampleInput)
        puzzleInput  <- fileLines(Path(s"data/$day/puzzle-1.txt"))
        puzzleResult  = resolveStar1(puzzleInput)
      } yield assertTrue(
        exampleResult == 2,
        puzzleResult == 503
      )
    },
    test("star#2") {
      for {
        exampleInput <- fileLines(Path(s"data/$day/example-1.txt"))
        exampleResult = resolveStar2(exampleInput)
        puzzleInput  <- fileLines(Path(s"data/$day/puzzle-1.txt"))
        puzzleResult  = resolveStar2(puzzleInput)
      } yield assertTrue(
        exampleResult == 4,
        puzzleResult == 827
      )
    }
  ) @@ timed @@ sequential
}
