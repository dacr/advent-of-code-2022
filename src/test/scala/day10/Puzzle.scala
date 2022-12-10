package day10

import zio.*
import zio.stream.*
import zio.test.*
import zio.test.TestAspect.*

type Numeric = Int

// ------------------------------------------------------------------------------
def parseLine(line: String): Stream[Throwable, Numeric] =
  line.split(" ", 2) match {
    case a if a.length == 1 => ZStream(0)
    case Array(_, value)    => ZStream(0, value.toInt)
  }

// ------------------------------------------------------------------------------

def resolveStar1_alt1(lines: Stream[Throwable, String]) = {
  val cycles = Set(20, 60, 100, 140, 180, 220)

  lines
    .flatMap(parseLine)
    .forever
    .scan(1 -> 1) { case ((cycle, value), inc) => (cycle + 1) -> (value + inc) }
    .collect { case (cycle, value) if cycles.contains(cycle) => cycle * value }
    .take(cycles.size)
    .runSum
}

def resolveStar1_alt2(lines: Stream[Throwable, String]) = {
  lines
    .flatMap(parseLine)
    .forever
    .scan(1 -> 1) { case ((cycle, value), inc) => (cycle + 1) -> (value + inc) }
    .drop(19)
    .map((cycle, value) => cycle * value)
    .zipWithIndex
    .collect { case (value, index) if index % 40 == 0 => value }
    .take(6)
    .runSum
}
// ------------------------------------------------------------------------------

def inRange(registerValue: Numeric, pixelNum: Int): String = {
  if (registerValue - 1 <= pixelNum && pixelNum <= registerValue + 1) "#" else "."
}

def resolveStar2(lines: Stream[Throwable, String]) =
  lines
    .flatMap(parseLine)
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
      val solver = resolveStar1_alt2
      for {
        exampleResult1 <- solver(fileLinesStream(Path(s"data/$day/example-1.txt")))
        puzzleResult   <- solver(fileLinesStream(Path(s"data/$day/puzzle-1.txt")))
      } yield assertTrue(
        exampleResult1 == 13140,
        puzzleResult == 14560
      )
    },
    test("star#2") {
      for {
        exampleResult1 <- resolveStar2(fileLinesStream(Path(s"data/$day/example-1.txt")))
        puzzleResult   <- resolveStar2(fileLinesStream(Path(s"data/$day/puzzle-1.txt")))
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
  ) @@ timed @@ sequential
}
