package day15

import zio.*
import zio.test.*
import zio.test.TestAspect.*
import scala.math.*

// ------------------------------------------------------------------------------
case class Range(a: Long, b: Long) {
  def contains(x: Long) = a <= x && x <= b
}

case class Coord(x: Long, y: Long)

def manhattanDistance(from: Coord, to: Coord) = abs(from.x - to.x) + abs(from.y - to.y)

case class Zone(sensor: Coord, closestBeacon: Coord) {
  val radius               = manhattanDistance(sensor, closestBeacon)
  def contains(pos: Coord) = manhattanDistance(pos, sensor) <= radius

  def rangeAtRow(y: Long): Option[Range] = {
    val delta = abs(sensor.y - y)
    if (delta > radius) None
    else Some(Range(sensor.x - (radius - delta), sensor.x + (radius - delta)))
  }

  override def toString: String = s"(${sensor.x},${sensor.y}) r=$radius"
}

// ------------------------------------------------------------------------------
val lineRE = """Sensor at x=([-]?\d+), y=([-]?\d+): closest beacon is at x=([-]?\d+), y=([-]?\d+)""".r

def parse(input: List[String]) =
  input.collect { case lineRE(sx, sy, bx, by) => Zone(Coord(sx.toLong, sy.toLong), Coord(bx.toLong, by.toLong)) }.toVector

// ------------------------------------------------------------------------------
def noBeaconPlaceCount(zones: Vector[Zone], y: Long): Int = {
  val beacons = zones.map(_.closestBeacon).toSet
  val sensors = zones.map(_.sensor).toSet
  val ranges  = zones.flatMap(_.rangeAtRow(y))
  val minX    = ranges.map(_.a).min
  val maxX    = ranges.map(_.b).max

  val result = minX.to(maxX).filter { x =>
    val coord = Coord(x, y)
    !beacons.contains(coord) &&
    !sensors.contains(coord) &&
    ranges.exists(_.contains(x))
  }
  result.size
}

def resolveStar1(input: List[String], row: Int): Int =
  val zones = parse(input)
  // println(zones.mkString("\n"))
  noBeaconPlaceCount(zones, row)

// ------------------------------------------------------------------------------

def resolveStar2(input: List[String]): Int =
  0

// ------------------------------------------------------------------------------

object Puzzle15Test extends ZIOSpecDefault {
  import zio.nio.file.Path
  import helpers.Helpers.*
  val day  = getClass.getName.replaceAll(""".*Puzzle(\d+)Test.*""", "day$1")
  def spec = suite(s"puzzle $day")(
    test("parsing") {
      for {
        exampleInput <- fileLines(Path(s"data/$day/example-1.txt"))
        puzzleInput  <- fileLines(Path(s"data/$day/puzzle-1.txt"))
      } yield assertTrue(
        exampleInput.size == 14,
        puzzleInput.size == 32
      )
    },
    test("star#1") {
      for {
        exampleInput  <- fileLines(Path(s"data/$day/example-1.txt"))
        exampleResult1 = resolveStar1(exampleInput, 10)
        puzzleInput   <- fileLines(Path(s"data/$day/puzzle-1.txt"))
        puzzleResult   = resolveStar1(puzzleInput, 2000000)
      } yield assertTrue(
        exampleResult1 == 26,
        puzzleResult == 5716881
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
