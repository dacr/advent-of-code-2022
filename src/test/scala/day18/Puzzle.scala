package day18

import zio.*
import zio.test.*
import zio.test.TestAspect.*
import scala.util.chaining.*

// ------------------------------------------------------------------------------
case class Coord(x: Int, y: Int, z: Int) {
  def left               = Coord(x - 1, y, z)
  def right              = Coord(x + 1, y, z)
  def up                 = Coord(x, y + 1, z)
  def down               = Coord(x, y - 1, z)
  def backward           = Coord(x, y, z + 1)
  def forward            = Coord(x, y, z - 1)
  def around: Set[Coord] = Set(left, right, up, down, backward, forward)
}

case class Face(coords: Set[Coord]) extends AnyVal

case class Droplet(faces: Set[Face]) extends AnyVal {
  def left     = Droplet(faces.map(face => Face(face.coords.map(_.left))))
  def right    = Droplet(faces.map(face => Face(face.coords.map(_.right))))
  def up       = Droplet(faces.map(face => Face(face.coords.map(_.up))))
  def down     = Droplet(faces.map(face => Face(face.coords.map(_.down))))
  def backward = Droplet(faces.map(face => Face(face.coords.map(_.backward))))
  def forward  = Droplet(faces.map(face => Face(face.coords.map(_.forward))))
}

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

def walk(droplets: Set[Droplet]): Int = {
  val allFaces  = droplets.flatMap(_.faces)
  val allCoords = allFaces.flatMap(_.coords)
  val minX      = allCoords.map(_.x).min - 1
  val maxX      = allCoords.map(_.x).max + 1
  val minY      = allCoords.map(_.y).min - 1
  val maxY      = allCoords.map(_.y).max + 1
  val minZ      = allCoords.map(_.z).min - 1
  val maxZ      = allCoords.map(_.z).max + 1
  val startFrom = Coord(minX, minY, minZ)

  @annotation.tailrec
  def worker(tovisit: List[Coord], visited: Set[Coord], unconnectedFaceCount: Int): Int = {
    tovisit match {
      case Nil => unconnectedFaceCount

      case head :: remain if visited.contains(head) => worker(remain, visited, unconnectedFaceCount)

      case head :: remain if droplets.contains(Droplet(head)) => worker(remain, visited + head, unconnectedFaceCount)

      case head :: remain =>
        val validNextCoords =
          head.around
            .filter(coord =>
              coord.x >= minX &&
                coord.x <= maxX &&
                coord.y >= minY &&
                coord.y <= maxY &&
                coord.z >= minZ &&
                coord.z <= maxZ
            )

        val (occupiedCoords, freeCoords) = validNextCoords.partition(coord => droplets.contains(Droplet(coord)))
        worker(remain ++ freeCoords.toList, visited + head, unconnectedFaceCount + occupiedCoords.size)
    }
  }

  worker(startFrom :: Nil, Set.empty, 0)
}

def resolveStar2(input: List[String]): Int =
  val droplets = parse(input)
  walk(droplets)

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
        puzzleResult == 2052
      )
    }
  ) @@ timed @@ sequential
}
