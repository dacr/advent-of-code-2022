package day16

import zio.*
import zio.test.*
import zio.test.TestAspect.*
import scala.math.*

// ------------------------------------------------------------------------------
// Written originally from DAY12
def shortestPath[COORD](from: COORD)(goalCheck: COORD => Boolean, nextMoves: COORD => List[COORD]): List[COORD] = {
  @annotation.tailrec
  def worker(visitQueue: List[(List[COORD], Set[COORD], Int)], bestDepths: Map[COORD, Int]): List[COORD] = {
    visitQueue match {
      case Nil => Nil

      case (current :: path, visited, depth) :: _ if goalCheck(current) =>
        // area.printRoute(current :: path, bestDepths)
        current :: path

      case (from :: path, visited, fromDepth) :: others if bestDepths.contains(from) && fromDepth >= bestDepths(from) =>
        worker(others, bestDepths)

      case (from :: path, visited, fromDepth) :: others =>
        val nextToVisit       = nextMoves(from).filterNot(visited.contains)
        val updatedBestDepths = bestDepths + (from -> fromDepth)
        val updatedVisited    = visited + from
        val updatedPath       = from :: path
        val updatedVisitQueue = others ++ nextToVisit.map(to => (to :: updatedPath, updatedVisited, fromDepth + 1))
        // area.printRoute(from :: path, updatedBestDepths)
        worker(updatedVisitQueue, updatedBestDepths)
    }
  }
  worker((from :: Nil, Set.empty, 0) :: Nil, Map.empty)
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
case class ShortestPath(from: Valve, to: Valve, path: List[ValveName], size: Int)

def search(origin: ValveName, valveByName: Map[ValveName, Valve], limit: Int = 30): Int = {
  val openableValves     = valveByName.values.filter(_.rate.value > 0).toSet
  val openableValvesName = openableValves.map(_.name)

  val shortestSegments =
    (openableValves + valveByName(origin)) // Add origin of course although it is not openable (rate == 0)
      .toList
      .combinations(2)
      .flatMap { case Seq(from, to) =>
        val path = shortestPath(from.name)(_ == to.name, name => valveByName(name).children)
        List(
          Segment(from.name, to.name) -> ShortestPath(from, to, path.reverse, path.size),
          Segment(to.name, from.name) -> ShortestPath(to, from, path, path.size)
        )
      }
      .filterNot { case (s, _) => s.to == origin }
      .toMap

  println(shortestSegments.map((s, p) => s -> p.path).toList.sortBy((s, p) => s.from.name).mkString("\n"))

  def worker(fromName: ValveName, depth: Int, opened: Set[ValveName], closedOpenable: Set[ValveName], openedSum: Int, accumulator: Int): Int = {
    if (depth == 3)  println(s"$depth : $accumulator $openedSum ${opened.map(_.name).mkString(",")}")
    if (depth == limit || closedOpenable.isEmpty) {
      val updatedAccumulator = accumulator + openedSum * (limit - min(limit, depth))
      updatedAccumulator
    } else {
      val fromValve = valveByName(fromName)

      lazy val childResults =
        shortestSegments.collect {
          case (segment, shortest) if segment.from == fromName && closedOpenable.contains(segment.to) =>
            val updatedDepth       = min(limit, depth + shortest.size - 1)
            val updatedAccumulator = accumulator + openedSum * (updatedDepth - depth)
            //println(s"$segment - $depth->$updatedDepth - $updatedAccumulator")
            worker(
              fromName = segment.to,
              depth = updatedDepth,
              opened = opened,
              closedOpenable = closedOpenable,
              openedSum = openedSum,
              accumulator = updatedAccumulator
            )
        }.toList

      val result = if (closedOpenable.contains(fromName)) {
        worker(
          fromName = fromName,
          depth = depth + 1,
          opened = opened + fromName,
          closedOpenable = closedOpenable - fromName,
          openedSum = openedSum + fromValve.rate.value,
          accumulator = accumulator + openedSum
        ) :: childResults
      } else {
        childResults :+ worker(
          fromName = fromName,
          depth = depth + 1,
          opened = opened,
          closedOpenable = closedOpenable,
          openedSum = openedSum,
          accumulator = accumulator + openedSum
        )
      }
      result.max
    }
  }
  worker(fromName = origin, depth = 1, opened = Set(), closedOpenable = openableValvesName, openedSum = 0, accumulator = 0)
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
