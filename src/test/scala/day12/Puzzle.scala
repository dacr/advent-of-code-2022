package day12

import zio.*
import zio.test.*
import zio.test.TestAspect.*
import scala.math.*
import scala.io.AnsiColor.{WHITE, BLUE, RESET, RED, BOLD}
import scala.util.chaining._

case class Coord(x: Int, y: Int)
case class Cell(height: Char)            extends AnyVal
case class Grid(cells: Map[Coord, Cell]) extends AnyVal
case class Area(grid: Grid, origin: Coord, goal: Coord, width: Int, height: Int) {
  def printRoute(route: List[Coord], bestDepth: Map[Coord, Int] = Map.empty): Unit = {
    val routeSet = route.toSet
    val lines    =
      0.until(height)
        .map(y =>
          0.until(width)
            .map { x =>
              val coord = Coord(x, y)
              val ch    = grid.cells(coord).height
              if (routeSet.contains(coord))
                s"$RED$BOLD$ch$RESET"
              else if (bestDepth.contains(coord))
                s"$BLUE$ch$RESET"
              else s"$ch"
            }
            .mkString
        )
    println()
    println(lines.mkString("\n"))
  }
}

// ------------------------------------------------------------------------------
def parse(input: List[String]): Area = {
  val width       = input.headOption.map(_.size).getOrElse(0)
  val height      = input.size
  val grid        = input.zipWithIndex.flatMap((line, y) => line.zipWithIndex.map((ch, x) => Coord(x, y) -> Cell(ch)))
  val origin      = grid.view.collect { case (coord, cell) if cell.height == 'S' => coord }.head
  val goal        = grid.view.collect { case (coord, cell) if cell.height == 'E' => coord }.head
  val updatedGrid = Grid(grid.toMap + (origin -> Cell('a')) + (goal -> Cell('z')))
  Area(updatedGrid, origin, goal, width, height)
}

// ------------------------------------------------------------------------------

def shortestPath[COORD](from: COORD)(goalCheck: COORD => Boolean, nextMoves: COORD => List[COORD]): List[COORD] = {
  @annotation.tailrec
  def worker(visitQueue: List[(List[COORD], Set[COORD], Int)], bestDepths: Map[COORD, Int]): List[COORD] = {
    visitQueue match {
      case Nil => Nil

      case (current :: path, visited, depth) :: _ if goalCheck(current) =>
        // area.printRoute(current :: path, bestDepths)
        path

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
def around(area: Area, from: Coord): List[Coord] =
  List(
    from.copy(x = from.x + 1),
    from.copy(x = from.x - 1),
    from.copy(y = from.y + 1),
    from.copy(y = from.y - 1)
  )

def nextMoves1(area: Area)(from: Coord): List[Coord] =
  around(area, from).filter { to =>
    area.grid.cells.contains(to) &&
    (area.grid.cells(to).height - area.grid.cells(from).height <= 1)
  }

def resolveStar1(input: List[String]): Int =
  val area = parse(input)
  val path = shortestPath(area.origin)(_ == area.goal, nextMoves1(area))
  // area.printRoute(path)
  path.size

// ------------------------------------------------------------------------------

def nextMoves2(area: Area)(from: Coord): List[Coord] =
  around(area, from).filter { to =>
    area.grid.cells.contains(to) &&
    (area.grid.cells(to).height - area.grid.cells(from).height >= -1)
  }

def resolveStar2(input: List[String]): Int =
  val area = parse(input)
  val path = shortestPath(area.goal)(pos => area.grid.cells(pos).height == 'a', nextMoves2(area))
  path.size

// ------------------------------------------------------------------------------

object Puzzle12Test extends ZIOSpecDefault {
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
        exampleResult == 31,
        puzzleResult == 456
      )
    },
    test("star#2") {
      for {
        exampleInput1 <- fileLines(Path(s"data/$day/example-1.txt"))
        exampleResult1 = resolveStar2(exampleInput1)
        puzzleInput   <- fileLines(Path(s"data/$day/puzzle-1.txt"))
        puzzleResult   = resolveStar2(puzzleInput)
      } yield assertTrue(
        exampleResult1 == 29,
        puzzleResult == 454
      )
    }
  ) @@ timed @@ sequential
}
