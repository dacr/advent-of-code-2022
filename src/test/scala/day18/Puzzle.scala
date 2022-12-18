package day18

import zio.*
import zio.test.*
import zio.test.TestAspect.*
import scala.util.chaining.*

// ------------------------------------------------------------------------------
case class Coord(x: Int, y: Int, z: Int) {
  def left     = Coord(x - 1, y, z)
  def right    = Coord(x + 1, y, z)
  def up       = Coord(x, y + 1, z)
  def down     = Coord(x, y - 1, z)
  def backward = Coord(x, y, z + 1)
  def forward  = Coord(x, y, z - 1)
}

case class Face(coords: Set[Coord]) extends AnyVal

case class Droplet(faces: Set[Face]) extends AnyVal

object Droplet {
  def apply(coord: Coord): Droplet = Droplet(
    Set(
      Face(Set(coord, coord.right, coord.right.down, coord.right.down.left)),
      Face(Set(coord.backward, coord.right.backward, coord.right.down.backward, coord.right.down.left.backward)),
      // ----------------
      Face(Set(coord, coord.backward, coord.backward.right, coord.backward.right.forward)),
      Face(Set(coord.down, coord.backward.down, coord.backward.right.down, coord.backward.right.forward.down)),
      // ----------------
      Face(Set(coord, coord.backward, coord.backward.down, coord.backward.down.forward)),
      Face(Set(coord.right, coord.backward.right, coord.backward.down.right, coord.backward.down.forward.right))
    )
  )
}

// ------------------------------------------------------------------------------
def parse(input: List[String]): Set[Droplet] =
  input
    .map(_.split(",", 3) match { case Array(x, y, z) => Coord(x.toInt, y.toInt, z.toInt) })
    .map(Droplet.apply)
    .toSet

// ------------------------------------------------------------------------------

def resolveStar1(input: List[String]): Int =
  val droplets = parse(input)
  droplets.toList // because we need to not count duplicates
    .flatMap(_.faces)
    .groupBy(x => x)
    .count((k, v) => v.size == 1)

// ------------------------------------------------------------------------------

def findInternalHoles(droplets: Set[Droplet]): Set[Droplet] = {
  val allFaces     = droplets.flatMap(_.faces)
  val allCoords    = allFaces.flatMap(_.coords)
  val fillWithHole =
    allCoords
      .map(coord => coord -> Droplet(coord))
      .filterNot((coord, droplet) => droplets.contains(droplet))
      .filter((coord, droplet) => droplet.faces.forall(allFaces.contains))
      .tapEach((coord, droplet) => println(s"FOUND HOLE AT : $coord"))
      .map((coord, droplet) => droplet)
  fillWithHole
}

def resolveStar2(input: List[String]): Int =
  val droplets      = parse(input)
  val internalHoles = findInternalHoles(droplets)


// ------------------------------------------------------------------------------

object Puzzle18Test extends ZIOSpecDefault {
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
        exampleResult == 64,
        puzzleResult == 3448
      )
    },
    test("star#2") {
      for {
        exampleInput1 <- fileLines(Path(s"data/$day/example-1.txt"))
        exampleResult1 = resolveStar2(exampleInput1)
        puzzleInput   <- fileLines(Path(s"data/$day/puzzle-1.txt"))
        puzzleResult   = resolveStar2(puzzleInput)
      } yield assertTrue(
        exampleResult1 == 58,
        puzzleResult > 1651,
        puzzleResult < 3244
      )
    }
  ) @@ timed @@ sequential
}
