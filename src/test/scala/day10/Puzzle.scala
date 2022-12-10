package day10

import zio.*
import zio.stream.*
import zio.test.*
import zio.test.TestAspect.*

type Numeric = Int

// ------------------------------------------------------------------------------
def parse(lines: List[String]): List[Numeric] =
  lines
    .map(line => line.split(" ", 2))
    .flatMap {
      case a if a.length == 1 => List(0)
      case Array(_, value)    => List(0, value.toInt)
    }

// ------------------------------------------------------------------------------

def resolveStar1(lines: List[String]) = {
  val cycles = Set(20, 60, 100, 140, 180, 220)

  ZStream
    .fromIterable(parse(lines))
    .forever
    .scan(1 -> 1) { case ((cycle, value), inc) => (cycle + 1) -> (value + inc) }
    .collect { case (cycle, value) if cycles.contains(cycle) => cycle * value }
    .take(cycles.size)
    .runSum
}
// ------------------------------------------------------------------------------

def inRange(registerValue: Numeric, pixelNum: Int): String = {
  if (registerValue - 1 <= pixelNum && pixelNum <= registerValue + 1) "#" else "."
}

def resolveStar2(lines: List[String]) =
  ZStream
    .fromIterable(parse(lines))
    .forever
    .scan(1 -> 1) { case ((cycle, value), inc) => (cycle + 1) -> (value + inc) }
    .map((_, value) => value)
    .grouped(40)
    .map(line => line.zipWithIndex.map(inRange).mkString)
    .take(6)
    .runCollect
    .map(_.mkString("\n"))

// ------------------------------------------------------------------------------

object Puzzle10Test extends ZIOSpecDefault {
  import zio.nio.file.Path
  import helpers.Helpers.*
  val day  = getClass.getName.replaceAll(""".*Puzzle(\d+)Test.*""", "day$1")
  def spec = suite(s"puzzle $day")(
    test("star#1") {
      for {
        exampleInput1  <- fileLines(Path(s"data/$day/example-1.txt"))
        exampleResult1 <- resolveStar1(exampleInput1)
        puzzleInput    <- fileLines(Path(s"data/$day/puzzle-1.txt"))
        puzzleResult   <- resolveStar1(puzzleInput)
      } yield assertTrue(
        exampleResult1 == 13140,
        puzzleResult == 14560
      )
    },
    test("star#2") {
      for {
        exampleInput1  <- fileLines(Path(s"data/$day/example-1.txt"))
        exampleResult1 <- resolveStar2(exampleInput1)
        puzzleInput    <- fileLines(Path(s"data/$day/puzzle-1.txt"))
        puzzleResult   <- resolveStar2(puzzleInput)
        _              <- Console.printLine(puzzleResult)
      } yield assertTrue(
        exampleResult1 ==
          """##..##..##..##..##..##..##..##..##..##..
            |###...###...###...###...###...###...###.
            |####....####....####....####....####....
            |#####.....#####.....#####.....#####.....
            |######......######......######......####
            |#######.......#######.......#######.....""".stripMargin,
        puzzleResult ==
          """####.#..#.###..#..#.####.###..#..#.####.
            |#....#.#..#..#.#..#.#....#..#.#..#....#.
            |###..##...#..#.####.###..#..#.#..#...#..
            |#....#.#..###..#..#.#....###..#..#..#...
            |#....#.#..#.#..#..#.#....#....#..#.#....
            |####.#..#.#..#.#..#.####.#.....##..####.""".stripMargin
      )
    }
  ) @@ timed
}
