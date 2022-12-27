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

  val width = {
    val xs = shape.map(_.x)
    xs.max - xs.min + 1
  }

  val height = {
    val ys = shape.map(_.y)
    ys.max - ys.min + 1
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

      val updatedY = updatedX.down
      if (fallingRock.coords(updatedY).intersect(occupied).isEmpty)
        worker(roundIndex, instructionIndex + 1, currentHighestY, updatedY, occupied)
      else {
        val fallingRockCoords = fallingRock.coords(updatedX)
        // val updatedHighestY   = max(fallingRockCoords.maxBy(_.y).y, currentHighestY)
        val updatedHighestY   = max(updatedX.y + fallingRock.height - 1, currentHighestY)
        val updatedOccupied   = if (roundIndex % 1000 == 0) {
          occupied.filter(_.y > updatedHighestY - 100) ++ fallingRockCoords
        } else occupied ++ fallingRockCoords

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

def computeDeltas(roundLimit: Long, instructions: IndexedSeq[Direction]): List[Long] = {
  @annotation.tailrec
  def worker(roundIndex: Long, instructionIndex: Int, currentHighestY: Long, rockLeftBottom: Coord, occupied: Set[Coord], Δlist: List[Long]): List[Long] = {
    if (roundIndex >= roundLimit) Δlist.reverse
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

      val updatedY = updatedX.down
      if (fallingRock.coords(updatedY).intersect(occupied).isEmpty)
        worker(roundIndex, instructionIndex + 1, currentHighestY, updatedY, occupied, Δlist)
      else {
        val fallingRockCoords = fallingRock.coords(updatedX)
        // val updatedHighestY   = max(fallingRockCoords.maxBy(_.y).y, currentHighestY)
        val updatedHighestY   = max(updatedX.y + fallingRock.height - 1, currentHighestY)
        val updatedOccupied   = if (roundIndex % 1000 == 0) {
          occupied.filter(_.y > updatedHighestY - 100) ++ fallingRockCoords
        } else occupied ++ fallingRockCoords

        val Δ = updatedHighestY - currentHighestY

        worker(roundIndex + 1, instructionIndex + 1, updatedHighestY, Coord(2, updatedHighestY + 4), updatedOccupied, Δ :: Δlist)
      }
    }
  }
  worker(0, 0, 0L, Coord(2, 4), 0.to(7).map(x => Coord(x, 0)).toSet, Nil)
}

case class Cycle(from: Int, len: Int, pattern: List[Long])

// not the fastest algorithm of course...
def searchCycle(values: List[Long], maxCycleLength: Int = 100, maxStartDelta: Int = 100, cycleSamples: Int = 10): Option[Cycle] = {
  LazyList
    .from(1)
    .take(maxCycleLength)
    .flatMap { case cycleLength =>
      LazyList.from(0).take(maxStartDelta).flatMap { case from =>
        values.drop(from).grouped(cycleLength).take(cycleSamples).distinct.toList match {
          case cycle :: Nil => Some(Cycle(from, cycle.length, cycle))
          case _            => None
        }
      }
    }
    .headOption
}

def resolveStar2(input: String): Long =
  val instructions = parse(input)
  val upTo         = 1_000_000_000_000L
  val sample       = computeDeltas(50_000L, instructions)
  searchCycle(values = sample, maxCycleLength = 2000, maxStartDelta = 200, cycleSamples = 4) match {
    case None        => 0L
    case Some(cycle) =>
      val beforeCycleCount = cycle.from
      sample.take(beforeCycleCount).sum +
        cycle.pattern.sum * ((upTo - beforeCycleCount) / cycle.len) +
        cycle.pattern.take(((upTo - beforeCycleCount) % cycle.len).toInt).sum
  }

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
        puzzleResult == 1560932944615L
      )
    }
  ) @@ timed @@ sequential
}
