package tmp

import java.time.temporal.ChronoUnit
import math.*

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
  case _ :: Nil => positions

  case first :: second :: _ if abs(first.x - second.x) <= 1 && abs(first.y - second.y) <= 1 =>
    positions

  case first :: second :: tail =>
    val dx = (first.x - second.x) match {
      case 0                  => 0
      case delta if delta > 0 => +1
      case delta if delta < 0 => -1
    }
    val dy = (first.y - second.y) match {
      case 0                  => 0
      case delta if delta > 0 => +1
      case delta if delta < 0 => -1
    }
    first :: adjust(Pos(second.x + dx, second.y + dy) :: tail)
}

@annotation.tailrec
def applyMove(rope: Rope, instruction: Move, history: List[Pos]): (Rope, List[Pos]) = {
  instruction match {
    case Move(_, 0)             => (rope, history)
    case Move(direction, count) =>
      val newHead = rope.positions.head.move(direction)
      val newRope = Rope(adjust(newHead :: rope.positions.tail))
      val lastPos = newRope.positions.last
      applyMove(newRope, Move(direction, count - 1), lastPos :: history)
  }
}

// ------------------------------------------------------------------------------

@annotation.tailrec
def motionInAction(rope: Rope, moves: List[Move], visited: List[Pos]): List[Pos] = {
  moves match {
    case Nil                    => visited
    case move :: remainingMoves =>
      val (updatedRope, updatedVisited) = applyMove(rope, move, visited)
      motionInAction(updatedRope, remainingMoves, updatedVisited)
  }
}

def resolveStar1(input: List[String]): Int = {
  val moves     = parse(input)
  val startPos  = Pos(0, 0)
  val positions = List.fill(2)(Pos(0, 0))
  val visited   = motionInAction(Rope(positions), moves, List(startPos))
  visited.toSet.size
}

// ------------------------------------------------------------------------------

def resolveStar2(input: List[String]): Int = {
  val moves     = parse(input)
  val startPos  = Pos(0, 0)
  val positions = List.fill(10)(startPos)
  val visited   = motionInAction(Rope(positions), moves, List(startPos))
  visited.toSet.size
}

object NativeTry {

  val day = "day09"

  def main(args: Array[String]) = {
    import java.nio.file.Files.readAllLines
    import java.nio.file.Path
    import java.io.File
    import scala.jdk.CollectionConverters.*

    val started       = System.currentTimeMillis
    val puzzleInput   = readAllLines(File(s"data/$day/puzzle-1.txt").toPath)
    val puzzleResult  = resolveStar2(puzzleInput.asScala.toList)
    val ended         = System.currentTimeMillis

    System.out.println(s"In ${ended - started}ms")

    assert(
        puzzleResult == 2327
    )
  }

}
