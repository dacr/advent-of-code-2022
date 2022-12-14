package day14

import zio.*
import zio.test.*
import zio.test.TestAspect.*
import scala.math.*

// ------------------------------------------------------------------------------
enum Cell(val char: Char) {
  case rock extends Cell('#')
  case sand extends Cell('o')
}
case class Coord(x: Int, y: Int)                                                             {
  def down      = Coord(x, y + 1)
  def left      = Coord(x - 1, y)
  def right     = Coord(x + 1, y)
  def leftDown  = Coord(x - 1, y + 1)
  def rightDown = Coord(x + 1, y + 1)
}
case class Reservoir(grid: Map[Coord, Cell], origin: Coord, xMin: Int, xMax: Int, yMax: Int) {
  def dump(current: Option[Coord] = None) = {
    val xMin = grid.keys.minBy(_.x).x
    val xMax = grid.keys.maxBy(_.x).x
    val yMax = grid.keys.maxBy(_.y).y
    0.to(yMax)
      .map { y =>
        val line = xMin
          .to(xMax)
          .map { x =>
            val coord = Coord(x, y)
            grid.get(coord) match {
              case None if current.isDefined && current.get == coord => "O"
              case None                                              => "."
              case Some(cell)                                        => cell.char
            }
          }
          .mkString
        println(line)
      }
  }
}

object Coord {
  def fromString(input: String) =
    input.trim.split(",", 2) match {
      case Array(x, y) => Coord(x.toInt, y.toInt)
    }
}

// ------------------------------------------------------------------------------

def draw(from: Coord, to: Coord): Iterable[Coord] = {
  val dx = signum(to.x - from.x)
  val dy = signum(to.y - from.y)
  if (dx != 0) from.x.to(to.x, dx).map(x => Coord(x, from.y))
  else from.y.to(to.y, dy).map(y => Coord(from.x, y))
}

def pathCoords(path: List[Coord]): Set[Coord] =
  path.sliding(2).flatMap { case List(from, to) => draw(from, to) }.toSet

def parse(input: List[String]): Reservoir = {
  val rocks =
    input
      .map(_.split(" -> ").toList.map(Coord.fromString))
      .flatMap(pathCoords)
      .toSet
  val xMin  = rocks.minBy(_.x).x
  val xMax  = rocks.maxBy(_.x).x
  val yMax  = rocks.maxBy(_.y).y
  val grid  = rocks.map(coord => coord -> Cell.rock).toMap
  Reservoir(grid, Coord(500, 0), xMin, xMax, yMax)
}

// ------------------------------------------------------------------------------

@annotation.tailrec
def simulate(reservoir: Reservoir, turn: Int): Int = {
  def checkAbyss(coord: Coord)                 = coord.x < reservoir.xMin || coord.x > reservoir.xMax
  def freeDown(coord: Coord)                   = !reservoir.grid.contains(coord.down)
  def freeLeftDown(coord: Coord)               = checkAbyss(coord.leftDown) || !reservoir.grid.contains(coord.leftDown)
  def freeRightDown(coord: Coord)              = checkAbyss(coord.rightDown) || !reservoir.grid.contains(coord.rightDown)
  @annotation.tailrec
  def findPlace(current: Coord): Option[Coord] = {
    // reservoir.dump(Some(current))
    current match {
      case coord if checkAbyss(coord) => None

      case coord if freeDown(coord) => findPlace(coord.down)

      case coord if freeLeftDown(coord) => findPlace(coord.leftDown)

      case coord if freeRightDown(coord) => findPlace(coord.rightDown)

      case coord => Some(coord)
    }
  }
  findPlace(reservoir.origin) match {
    case Some(coord) =>
      val updatedGrid      = reservoir.grid.updated(coord, Cell.sand)
      val updatedReservoir = reservoir.copy(grid = updatedGrid)
      simulate(updatedReservoir, turn + 1)
    case None        => turn
  }
}

// ------------------------------------------------------------------------------

def resolveStar1(input: List[String]): Int =
  val reservoir = parse(input)
  simulate(reservoir, 1) - 1

// ------------------------------------------------------------------------------

@annotation.tailrec
def simulate2(reservoir: Reservoir, turn: Int): Int = {
  def freeDown(coord: Coord)                   = !reservoir.grid.contains(coord.down) && coord.y < reservoir.yMax + 1
  def freeLeftDown(coord: Coord)               = !reservoir.grid.contains(coord.leftDown) && coord.y < reservoir.yMax + 1
  def freeRightDown(coord: Coord)              = !reservoir.grid.contains(coord.rightDown) && coord.y < reservoir.yMax + 1
  @annotation.tailrec
  def findPlace(current: Coord): Option[Coord] = {
    //reservoir.dump(Some(current))
    current match {
      case coord if reservoir.grid.contains(reservoir.origin) => None

      case coord if freeDown(coord) => findPlace(coord.down)

      case coord if freeLeftDown(coord) => findPlace(coord.leftDown)

      case coord if freeRightDown(coord) => findPlace(coord.rightDown)

      case coord => Some(coord)
    }
  }
  findPlace(reservoir.origin) match {
    case Some(coord) =>
      val updatedGrid      = reservoir.grid.updated(coord, Cell.sand)
      val updatedReservoir = reservoir.copy(grid = updatedGrid)
      simulate2(updatedReservoir, turn + 1)
    case None        => turn
  }
}

def resolveStar2(input: List[String]): Int =
  val reservoir = parse(input)
  simulate2(reservoir, 1) -1

// ------------------------------------------------------------------------------

object Puzzle14Test extends ZIOSpecDefault {
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
        exampleResult == 24,
        puzzleResult == 696
      )
    },
    test("star#2") {
      for {
        exampleInput1 <- fileLines(Path(s"data/$day/example-1.txt"))
        exampleResult1 = resolveStar2(exampleInput1)
        puzzleInput   <- fileLines(Path(s"data/$day/puzzle-1.txt"))
        puzzleResult   = resolveStar2(puzzleInput)
      } yield assertTrue(
        exampleResult1 == 93,
        puzzleResult == 23610
      )
    }
  ) @@ timed @@ sequential
}
