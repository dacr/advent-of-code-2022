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
  case first :: second :: tail if abs(first.x - second.x) > 1 || abs(first.y - second.y) > 1 =>
    val dx = signum(first.x - second.x)
    val dy = signum(first.y - second.y)
    first :: adjust(Pos(second.x + dx, second.y + dy) :: tail)

  case _ => positions
}

@annotation.tailrec
def applyMove(rope: Rope, instruction: Move, history: List[Pos]): (Rope, List[Pos]) = {
  import instruction.*
  if (howMany == 0) (rope, history)
  else {
    val newHead = rope.positions.head.move(direction)
    val newRope = Rope(adjust(newHead :: rope.positions.tail))
    val lastPos = newRope.positions.last
    applyMove(newRope, Move(direction, howMany - 1), lastPos::history)
  }
}

// ------------------------------------------------------------------------------

@annotation.tailrec
def motionInAction(rope: Rope, moves: List[Move], visited: List[Pos]): List[Pos] = {
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
  val visited   = motionInAction(Rope(positions), moves, List(startPos))
  visited.distinct.size
}

// ------------------------------------------------------------------------------

def resolveStar2(input: List[String]): Int = {
  val moves     = parse(input)
  val startPos  = Pos(0, 0)
  val positions = List.fill(10)(startPos)
  val visited   = motionInAction(Rope(positions), moves, List(startPos))
  visited.distinct.size
}

object PerfTry {

  val day = "day09"

  def main(args: Array[String]) = {
    import java.nio.file.Files.readAllLines
    import java.nio.file.Path
    import java.io.File
    import scala.jdk.CollectionConverters.*


    1.to(4000).foreach { i =>
      val puzzleInput = readAllLines(File(s"data/$day/puzzle-1.txt").toPath).asScala.toList
      val puzzleResult = resolveStar2(puzzleInput)
      assert(puzzleResult == 2327)
    }

    val started       = System.nanoTime()
    val puzzleInput = readAllLines(File(s"data/$day/puzzle-1.txt").toPath).asScala.toList
    val puzzleResult = resolveStar2(puzzleInput)
    val ended         = System.nanoTime()
    val duration      = (ended - started).toDouble / 1000L
    System.out.println(s"In ${duration}µs - $puzzleResult")
    assert(puzzleResult == 2327)


  }

}
