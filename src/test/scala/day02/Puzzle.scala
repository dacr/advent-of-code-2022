package day02

import zio.*
import zio.test.*
import zio.nio.file.Path
import helpers.Helpers.*

// ------------------------------------------------------------------------------
enum Move(val score: Int) {
  case Rock     extends Move(1)
  case Paper    extends Move(2)
  case Scissors extends Move(3)
}

enum State(val outcome: Int) {
  case Lost extends State(0)
  case Draw extends State(3)
  case Won  extends State(6)
}

import Move.*, State.*

val games = List(
  Game(Rock, Paper, Won),
  Game(Paper, Scissors, Won),
  Game(Scissors, Rock, Won),
  Game(Paper, Rock, Lost),
  Game(Scissors, Paper, Lost),
  Game(Rock, Scissors, Lost),
  Game(Rock, Rock, Draw),
  Game(Paper, Paper, Draw),
  Game(Scissors, Scissors, Draw)
)

case class Game(opponentMove: Move, playerMove: Move, state: State) {
  def score: Int = playerMove.score + state.outcome
}

def convertMove(input: String): Move = input match { // TODO of course dangerous
  case "A" | "X" => Rock
  case "B" | "Y" => Paper
  case "C" | "Z" => Scissors
}

def toGame1(input: String): Game = {
  input.trim.split(" ") match {
    case Array(opponent, player) =>
      val opponentMove = convertMove(opponent)
      val playerMove   = convertMove(player)
      games.find(game => game.opponentMove == opponentMove && game.playerMove == playerMove).get // TODO of course dangerous
  }
}

def convertState(input: String): State = input match { // TODO of course dangerous
  case "X" => Lost
  case "Y" => Draw
  case "Z" => Won
}

def toGame2(input: String): Game = {
  input.trim.split(" ") match {
    case Array(opponent, state) =>
      val opponentMove = convertMove(opponent)
      val goal         = convertState(state)
      games.find(game => game.opponentMove == opponentMove && game.state == goal).get // TODO of course dangerous
  }
}

def resolveStar1(input: List[String]): Int = {
  input
    .map(toGame1)
    .map(_.score)
    .sum
}
// ------------------------------------------------------------------------------

def resolveStar2(input: List[String]): Int = {
  input
    .map(toGame2)
    .map(_.score)
    .sum
}
// ------------------------------------------------------------------------------

object Puzzle02Test extends ZIOSpecDefault {
  val day  = getClass.getName.replaceAll(""".*Puzzle(\d+)Test.*""", "day$1")
  def spec = suite(s"puzzle $day")(
    test("star#1") {
      for {
        exampleInput <- fileLines(Path(s"data/$day/example-1.txt"))
        exampleResult = resolveStar1(exampleInput)
        puzzleInput  <- fileLines(Path(s"data/$day/puzzle-1.txt"))
        puzzleResult  = resolveStar1(puzzleInput)
      } yield assertTrue(
        exampleResult == 15,
        puzzleResult == 14264
      )
    },
    test("star#2") {
      for {
        exampleInput <- fileLines(Path(s"data/$day/example-1.txt"))
        exampleResult = resolveStar2(exampleInput)
        puzzleInput  <- fileLines(Path(s"data/$day/puzzle-1.txt"))
        puzzleResult  = resolveStar2(puzzleInput)
      } yield assertTrue(
        exampleResult == 12,
        puzzleResult == 12382
      )
    }
  )
}
