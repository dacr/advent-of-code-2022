package day16

import zio.*
import zio.test.*
import zio.test.TestAspect.*
import scala.math.*

// ------------------------------------------------------------------------------
// Written originally from DAY12 - enhanced release
def simpleShortestPath[NODE](from: NODE)(goalCheck: NODE => Boolean, nextMoves: NODE => List[NODE]): List[NODE] = {
  case class Work(builtReversePath: List[NODE], visited: Set[NODE], currentDepth: Int)
  @annotation.tailrec
  def worker(workQueue: List[Work], bestDepths: Map[NODE, Int]): List[NODE] = {
    workQueue match {
      case Nil => Nil

      case Work(current :: path, _, _) :: _ if goalCheck(current) =>
        current :: path

      case Work(from :: _, _, fromDepth) :: others if bestDepths.contains(from) && fromDepth >= bestDepths(from) =>
        worker(others, bestDepths)

      case Work(from :: path, visited, fromDepth) :: others =>
        val nextToVisit       = nextMoves(from).filterNot(visited.contains)
        val updatedBestDepths = bestDepths + (from -> fromDepth)
        val updatedVisited    = visited + from
        val updatedPath       = from :: path
        val updatedVisitQueue = others ++ nextToVisit.map(to => Work(to :: updatedPath, updatedVisited, fromDepth + 1))
        worker(updatedVisitQueue, updatedBestDepths)
    }
  }

  val startWork = Work(from :: Nil, Set.empty, 0)

  worker(startWork :: Nil, Map.empty)
}

// ------------------------------------------------------------------------------
case class ValveName(name: String) extends AnyVal {
  override def toString: String = name
}
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
case class Segment(from: ValveName, to: ValveName)
case class ShortestPath(from: Valve, to: Valve, size: Int)

def search(originName: ValveName, valveByName: Map[ValveName, Valve], limit: Int = 30): Int = {
  val openableValves     = valveByName.values.filter(_.rate.value > 0).toSet
  val openableValvesName = openableValves.map(_.name)

  val graph =
    (openableValves + valveByName(originName)) // Add origin of course although it is not openable (rate == 0)
      .toList
      .combinations(2)
      .flatMap { case Seq(from, to) =>
        val path = simpleShortestPath(from.name)(_ == to.name, name => valveByName(name).children)
        List(
          Segment(from.name, to.name) -> ShortestPath(from, to, path.size),
          Segment(to.name, from.name) -> ShortestPath(to, from, path.size)
        )
      }
      .filterNot { case (s, _) => s.to == originName }
      .toMap

  println(graph.map((s, p) => s -> p.size).toList.sortBy((s, p) => s.from.name).mkString("\n"))

  def worker(fromName: ValveName): Int = {
    0
  }
  worker(originName)
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
