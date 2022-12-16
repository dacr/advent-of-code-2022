package day16

import zio.*
import zio.test.*
import zio.test.TestAspect.*

// ------------------------------------------------------------------------------
case class ValveName(name: String) extends AnyVal
case class ValveRate(value: Int)   extends AnyVal
case class Valve(name: ValveName, rate: ValveRate, children: List[ValveName])

// ------------------------------------------------------------------------------
val lineRE = """Valve (\w+) has flow rate=(\d+); tunnels? leads? to valves? ([, A-Z]+)""".r

def parse(input: List[String]): Map[ValveName, Valve] =
  input
    .collect { case lineRE(name, rate, children) =>
      Valve(ValveName(name), ValveRate(rate.toInt), children.split(""",\s*""").map(ValveName).toList)
    }
    .map(valve => valve.name -> valve)
    .toMap

// ------------------------------------------------------------------------------
def search(from: ValveName, valves: Map[ValveName, Valve]): Int = {
  val openableValve = valves.values.count(_.rate.value > 0)

  def worker(current: ValveName, depth: Int, opened: Set[ValveName], openedSum: Int, accumulator: Int): Int = {
    if (opened.size == openableValve || depth > 30) {
      println(s"$depth $accumulator")
      accumulator + (openedSum * (30 - depth - 1))
    } else {
      val valve              = valves(current)
      val updatedAccumulator = accumulator + openedSum
      val updatedDepth       = depth + 1

      lazy val childResults = valve.children.map(childName => worker(childName, updatedDepth, opened, openedSum, updatedAccumulator))

      val result = if (valve.rate.value == 0 || opened.contains(valve.name)) {
        childResults
      } else {
        worker(current, updatedDepth, opened + current, openedSum + valve.rate.value, updatedAccumulator) :: childResults
      }
      result.max
    }
  }
  worker(from, depth = 1, opened = Set.empty, openedSum = 0, accumulator = 0)
}

// ------------------------------------------------------------------------------

def resolveStar1(input: List[String]): Int =
  val valves = parse(input)
  search(ValveName("AA"), valves)

// ------------------------------------------------------------------------------

def resolveStar2(input: List[String]): Int =
  0

// ------------------------------------------------------------------------------

object Puzzle16Test extends ZIOSpecDefault {
  import zio.nio.file.Path
  import helpers.Helpers.*
  val day  = getClass.getName.replaceAll(""".*Puzzle(\d+)Test.*""", "day$1")
  def spec = suite(s"puzzle $day")(
    test("parsing") {
      for {
        exampleInput <- fileLines(Path(s"data/$day/example-1.txt"))
        result        = parse(exampleInput)
      } yield assertTrue(
        result.size == 10,
        result.values.forall(_.children.size > 0)
      )
    },
    test("star#1") {
      for {
        exampleInput <- fileLines(Path(s"data/$day/example-1.txt"))
        exampleResult = resolveStar1(exampleInput)
        puzzleInput  <- fileLines(Path(s"data/$day/puzzle-1.txt"))
        // puzzleResult  = resolveStar1(puzzleInput)
      } yield assertTrue(
        exampleResult == 1651
        // puzzleResult == 0
      )
    },
    test("star#2") {
      for {
        exampleInput1 <- fileLines(Path(s"data/$day/example-1.txt"))
        exampleResult1 = resolveStar2(exampleInput1)
        puzzleInput   <- fileLines(Path(s"data/$day/puzzle-1.txt"))
        puzzleResult   = resolveStar2(puzzleInput)
      } yield assertTrue(
        exampleResult1 == 0,
        puzzleResult == 0
      )
    }
  ) @@ timed @@ sequential
}
