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
def heightCheck(from: Int, to: Int): Boolean = (to - from <= 1) //|| (to - from == 0)

@annotation.tailrec
def shortestPath(area: Area, visitQueue: List[(List[Coord], Set[Coord], Int)], bestDepths: Map[Coord, Int]): List[Coord] = {
  visitQueue match {
    case Nil => Nil

    case (current :: path, visited, depth) :: _ if current == area.goal =>
      // area.printRoute(current :: path, bestDepths)
      path

    case (from :: path, visited, fromDepth) :: others if bestDepths.contains(from) && fromDepth >= bestDepths(from) =>
      shortestPath(area, others, bestDepths)

    case (from :: path, visited, fromDepth) :: others =>
      val nextToVisit       = List(
        from.copy(x = from.x + 1),
        from.copy(x = from.x - 1),
        from.copy(y = from.y + 1),
        from.copy(y = from.y - 1)
      ).filter { to =>
        !visited.contains(to) &&
        area.grid.cells.contains(to) &&
        heightCheck(area.grid.cells(from).height, area.grid.cells(to).height)
      }
      val updatedBestDepths = bestDepths + (from -> fromDepth)
      val updatedVisited    = visited + from
      val updatedPath       = from :: path
      val updatedVisitQueue = others ++ nextToVisit.map(to => (to :: updatedPath, updatedVisited, fromDepth + 1))
      // area.printRoute(from :: path, updatedBestDepths)
      shortestPath(area, updatedVisitQueue, updatedBestDepths)
  }
}

def resolveStar1(input: List[String]): Int =
  val area = parse(input)
  val path = shortestPath(area, (area.origin :: Nil, Set.empty, 0) :: Nil, Map.empty)
  // area.printRoute(path)
  path.size

// ------------------------------------------------------------------------------

def resolveStar2(input: List[String]): Int =
  val area = parse(input)
  area.grid.cells
    .collect { case (pos, cell) if cell.height == 'a' => pos }
    .map(startPos => shortestPath(area, (startPos :: Nil, Set.empty, 0) :: Nil, Map.empty))
    .filter(_.size > 0)
    .minBy(_.size)
    // .tap(foundPath => area.printRoute(foundPath))
    .size

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
