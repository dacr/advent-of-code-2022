package day09

import zio.*
import zio.test.*
import zio.test.TestAspect.*
import math._

enum Direction(val code: String, val dx: Int, val dy: Int) {
  case Right extends Direction("R", 1, 0)
  case Left  extends Direction("L", -1, 0)
  case Up    extends Direction("U", 0, -1)
  case Down  extends Direction("D", 0, 1)
}
case class Move(direction: Direction, howMany: Int)

case class Pos(x: Int, y: Int) {
  def move(direction: Direction): Pos =
    Pos(x + direction.dx, y + direction.dy)
}

case class Rope(positions: List[Pos])

// ------------------------------------------------------------------------------
def parse(input: List[String]) =
  input.map { line =>
    line.split(" ", 2) match {
      case Array(code, n) => Move(Direction.values.find(_.code == code).get, n.toInt)
    }
  }

// ------------------------------------------------------------------------------

def adjust(positions: List[Pos]): List[Pos] = positions match {
  case first :: second :: tail if abs(first.x - second.x) > 1 || abs(first.y - second.y) > 1 =>
    val dx = signum(first.x - second.x)
    val dy = signum(first.y - second.y)
    first :: adjust(Pos(second.x + dx, second.y + dy) :: tail)

  case _ => positions
}

@annotation.tailrec
def applyMove(rope: Rope, instruction: Move, history: Set[Pos]): (Rope, Set[Pos]) = {
  import instruction.*
  if (howMany == 0) (rope, history)
  else {
    val newHead = rope.positions.head.move(direction)
    val newRope = Rope(adjust(newHead :: rope.positions.tail))
    val lastPos = newRope.positions.last
    applyMove(newRope, Move(direction, howMany - 1), history + lastPos)
  }
}

// ------------------------------------------------------------------------------

@annotation.tailrec
def motionInAction(rope: Rope, moves: List[Move], visited: Set[Pos]): Set[Pos] = {
  if (moves.isEmpty) visited
  else {
    val (updatedRope, updatedVisited) = applyMove(rope, moves.head, visited)
    motionInAction(updatedRope, moves.tail, updatedVisited)
  }
}

def resolveStar1(input: List[String]): Int = {
  val moves     = parse(input)
  val startPos  = Pos(0, 0)
  val positions = List.fill(2)(Pos(0, 0))
  val visited   = motionInAction(Rope(positions), moves, Set(startPos))
  visited.size
}

// ------------------------------------------------------------------------------

def resolveStar2(input: List[String]): Int = {
  val moves     = parse(input)
  val startPos  = Pos(0, 0)
  val positions = List.fill(10)(startPos)
  val visited   = motionInAction(Rope(positions), moves, Set(startPos))
  visited.size
}

// ------------------------------------------------------------------------------

//@org.junit.runner.RunWith(classOf[zio.test.junit.ZTestJUnitRunner])
object Puzzle09Test extends ZIOSpecDefault {
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
        exampleResult == 13,
        puzzleResult == 5960
      )
    },
    test("star#2") {
      for {
        exampleInput1 <- fileLines(Path(s"data/$day/example-1.txt"))
        exampleResult1 = resolveStar2(exampleInput1)
        exampleInput2 <- fileLines(Path(s"data/$day/example-2.txt"))
        exampleResult2 = resolveStar2(exampleInput2)
        puzzleInput   <- fileLines(Path(s"data/$day/puzzle-1.txt"))
        puzzleResult   = resolveStar2(puzzleInput)
      } yield assertTrue(
        exampleResult1 == 1,
        exampleResult2 == 36,
        puzzleResult == 2327
      )
    }
  ) @@ timed
}
