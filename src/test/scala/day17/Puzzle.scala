package day17

import zio.*
import zio.stream.*
import zio.test.*
import zio.test.TestAspect.*
import scala.math.*

// ------------------------------------------------------------------------------
enum Direction {
  case Left
  case Right
}

case class Coord(x: Long, y: Long) {
  def right = copy(x = x + 1)
  def left  = copy(x = x - 1)
  def down  = copy(y = y - 1)
}

case class RelativeCoord(x: Int, y: Int) {
  def from(coord: Coord): Coord = Coord(x + coord.x, y + coord.y)
}

case class Rock(shape: Set[RelativeCoord]) {
  def coords(leftBottom: Coord) = shape.map(_.from(leftBottom))
  val width                     = {
    val xs = shape.map(_.x)
    xs.max - xs.min + 1
  }
}

// of course a faster implementation would be to use an binary approach
object Rock {
  val rocks = Vector(
    Rock(Set(RelativeCoord(0, 0), RelativeCoord(1, 0), RelativeCoord(2, 0), RelativeCoord(3, 0))),
    Rock(Set(RelativeCoord(1, 0), RelativeCoord(1, 1), RelativeCoord(1, 2), RelativeCoord(0, 1), RelativeCoord(2, 1))),
    Rock(Set(RelativeCoord(0, 0), RelativeCoord(1, 0), RelativeCoord(2, 0), RelativeCoord(2, 1), RelativeCoord(2, 2))),
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

def simulate(roundLimit: Long, instructions: IndexedSeq[Direction]): Long = {
  @annotation.tailrec
  def worker(roundIndex: Long, instructionIndex: Int, currentHighestY: Long, rockLeftBottom: Coord, occupied: Set[Coord]): Long = {
    if (roundIndex >= roundLimit) currentHighestY
    else {
      val fallingRock = Rock.rocks((roundIndex % Rock.rocks.length).toInt)

      val updatedX = instructions(instructionIndex % instructions.size) match {
        case Direction.Right if rockLeftBottom.x + fallingRock.width < 7 =>
          val rr = rockLeftBottom.right
          if (fallingRock.coords(rr).intersect(occupied).isEmpty) rr else rockLeftBottom

        case Direction.Left if rockLeftBottom.x > 0 =>
          val rl = rockLeftBottom.left
          if (fallingRock.coords(rl).intersect(occupied).isEmpty) rl else rockLeftBottom

        case _ => rockLeftBottom
      }

      val updatedY          = updatedX.down
      if (fallingRock.coords(updatedY).intersect(occupied).isEmpty)
        worker(roundIndex, instructionIndex + 1, currentHighestY, updatedY, occupied)
      else {
        val fallingRockCoords = fallingRock.coords(updatedX)
        val updatedOccupied = occupied ++ fallingRockCoords
        val updatedHighestY = max(fallingRockCoords.maxBy(_.y).y, currentHighestY)
        println(s"$roundIndex $updatedHighestY")
        worker(roundIndex + 1, instructionIndex + 1, updatedHighestY, Coord(2, updatedHighestY + 4), updatedOccupied)
      }
    }
  }
  worker(0, 0, 0L, Coord(2, 4), 0.to(7).map(x => Coord(x, 0)).toSet)
}

def resolveStar1(input: String): Long =
  val instructions = parse(input)
  simulate(2022, instructions)

// ------------------------------------------------------------------------------

def resolveStar2(input: String): Long =
  val instructions = parse(input)
  simulate(1_000_000_000_000L, instructions)

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
        exampleResult == 3068L,
        puzzleResult == 3163L
      )
    },
    test("star#2") {
      for {
        exampleInput1 <- fileContent(Path(s"data/$day/example-1.txt"))
        exampleResult1 = resolveStar2(exampleInput1)
        puzzleInput   <- fileContent(Path(s"data/$day/puzzle-1.txt"))
        puzzleResult   = resolveStar2(puzzleInput)
      } yield assertTrue(
        exampleResult1 == 1514285714288L,
        puzzleResult == 0L
      )
    }
  ) @@ timed @@ sequential
}
