package day15

import zio.*
import zio.test.*
import zio.test.TestAspect.*
import scala.math.*

// ------------------------------------------------------------------------------
case class Range(from: Long, to: Long) {
  def contains(x: Long): Boolean = from <= x && x <= to

  def contains(that: Range): Boolean = from <= that.from && that.to <= to

  def overlap(that: Range): Boolean =
    (from <= that.to && that.to <= to) ||
      (from <= that.from && that.from <= to) ||
      (that.from <= to && to <= that.to) ||
      (that.from <= from && from <= that.to)

  def reduce(that: Range): Option[Range] = {
    if (contains(that)) Some(this)
    else if (that.contains(this)) Some(that)
    else if (overlap(that)) Some(Range(min(that.from, from), max(that.to, to)))
    else None
  }
}

def compactRanges(ranges: List[Range]): List[Range] = {
  def reduceWorker(remaining: List[Range]): List[Range] = {
    remaining match {
      case Nil =>
        Nil

      case a :: b :: rem if a.contains(b) =>
        reduceWorker(a :: rem)

      case a :: b :: rem if a.overlap(b) =>
        reduceWorker(Range(min(a.from, b.from), max(a.to, b.to)) :: rem)

      case a :: b :: rem if a.to + 1 == b.from =>
        reduceWorker(Range(min(a.from, b.from), max(a.to, b.to)) :: rem)

      case a :: rem =>
        a :: reduceWorker(rem)
    }
  }
  reduceWorker(ranges.sortBy(_.from))
}

def rangesOverlap(ranges: List[Range], that: Range): List[Range] = {
  ranges.filter(range => that.overlap(range))
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
def noBeaconPlace(zones: Vector[Zone], beacons: Set[Coord], sensors: Set[Coord], y: Long): Seq[Coord] = {
  val ranges = zones.flatMap(_.rangeAtRow(y))
  if (ranges.isEmpty) Seq.empty
  else {
    val minX = ranges.map(_.from).min
    val maxX = ranges.map(_.to).max

    minX.to(maxX).map(x => Coord(x, y)).filter { coord =>
      !beacons.contains(coord) &&
      !sensors.contains(coord) &&
      ranges.exists(_.contains(coord.x))
    }
  }
}

def resolveStar1(input: List[String], row: Int): Int =
  val zones   = parse(input)
  val beacons = zones.map(_.closestBeacon).toSet
  val sensors = zones.map(_.sensor).toSet
  // println(zones.sortBy(_.radius).mkString("\n"))
  noBeaconPlace(zones, beacons, sensors, row).size

// ------------------------------------------------------------------------------

def searchPlaces(zones: List[Zone], y: Long, maxX: Long): Seq[Long] = {
  val ranges = zones.flatMap(_.rangeAtRow(y))
  if (ranges.isEmpty) {
    Seq.empty
  } else {
    val toReduceRange   = Range(0L, maxX)
    val compactedRanges = compactRanges(ranges)
    if (compactedRanges.size > 1) { // TODO REFACTORING
      //println(s"$y:" + compactedRanges.mkString("->"))
      Seq((compactedRanges.head.to + 1) * 4000000 + y) // TODO REFACTORING!
    } else Seq.empty
  }
}

def resolveStar2(input: List[String]): Long = {
  val zones   = parse(input).toList
  val sensors = zones.map(_.sensor)
  val maxX    = sensors.maxBy(_.x).x
  val maxY    = sensors.maxBy(_.y).y
  //println(s"maxX=$maxX maxY=$maxY")
  val result  =
    0.to(maxY.toInt).flatMap(y => searchPlaces(zones, y, maxX))
  //println(result)
  result.head // TODO REFACTORING
}

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
        exampleResult1 == 56000011L,
        puzzleResult == 10852583132904L
      )
    }
  ) @@ timed @@ sequential
}
