package day17

import zio.*
import zio.stream.*
import zio.test.*
import zio.test.TestAspect.*

// ------------------------------------------------------------------------------
enum Direction {
  case Left
  case Right
}

case class Coord(x: Int, y: Int)

case class RelativeCoord(x: Int, y: Int) {
  def from(coord: Coord): Coord = Coord(x + coord.x, y + coord.y)
}

case class Rock(shape: Set[RelativeCoord]) {
  def coords(leftBottom: Coord) = shape.map(_.from(leftBottom))
}

object Rock {
  val rocks = Vector(
    Rock(Set(RelativeCoord(0, 0), RelativeCoord(1, 0), RelativeCoord(2, 0), RelativeCoord(3, 0))),
    Rock(Set(RelativeCoord(1, 0), RelativeCoord(1, 1), RelativeCoord(1, 2), RelativeCoord(0, 1), RelativeCoord(2, 1))),
    Rock(Set(RelativeCoord(2, 0), RelativeCoord(2, 1), RelativeCoord(2, 2), RelativeCoord(1, 2), RelativeCoord(0, 2))),
    Rock(Set(RelativeCoord(0, 0), RelativeCoord(0, 1), RelativeCoord(0, 2), RelativeCoord(0, 3))),
    Rock(Set(RelativeCoord(0, 0), RelativeCoord(1, 0), RelativeCoord(1, 1), RelativeCoord(0, 1)))
  )
}

// ------------------------------------------------------------------------------

def parse(input: String) =
  input
    .collect {
      case '<' => Direction.Left
      case '>' => Direction.Right
    }

// ------------------------------------------------------------------------------

def simulate(roundLimit: Int, instructions: IndexedSeq[Direction]): Int = {
  @annotation.tailrec
  def worker(roundIndex: Int, instructionIndex: Int, rockLeftBottom: Coord, occupied: Set[Coord]): Int = {
    val fallingRock = Rock.rocks(roundIndex)
    val updatedX = instructions(instructionIndex % instructions.size) match {
      case Direction.Right =>
      case Direction.Left =>
    }
    ???
  }
  worker(0, 0, Coord(2, 3), Set.empty)
}

def resolveStar1(input: String): Int =
  val instructions = parse(input)
  simulate(2022, instructions)

// ------------------------------------------------------------------------------

def resolveStar2(input: String): Int =
  0

// ------------------------------------------------------------------------------

object Puzzle17Test extends ZIOSpecDefault {
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
        exampleResult == 3068,
        puzzleResult == 0
      )
    },
    test("star#2") {
      for {
        exampleInput1 <- fileContent(Path(s"data/$day/example-1.txt"))
        exampleResult1 = resolveStar2(exampleInput1)
        puzzleInput   <- fileContent(Path(s"data/$day/puzzle-1.txt"))
        puzzleResult   = resolveStar2(puzzleInput)
      } yield assertTrue(
        exampleResult1 == 0,
        puzzleResult == 0
      )
    }
  ) @@ timed @@ sequential
}
