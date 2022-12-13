package day13

import zio.*
import zio.test.*
import zio.test.TestAspect.*
import scala.util.parsing.combinator.*

sealed trait Tree[C]
case class Leaf[C](content: C)              extends Tree[C]
case class Node[C](children: List[Tree[C]]) extends Tree[C]

// ------------------------------------------------------------------------------
object TreeParser extends RegexParsers {
  def open: Parser[Any] = """\[""".r

  def close: Parser[Any] = """\]""".r

  def number: Parser[Int] = """(\d+)""".r ^^ { _.toInt }

  def expr: Parser[Tree[Int]] =
    number ^^ { case n => Leaf(n) } | tree

  def tree: Parser[Tree[Int]] =
    open ~ close ^^ { _ => Node[Int](Nil) } |
      open ~ repsep(expr, ",") ~ close ^^ { case _ ~ children ~ _ => Node[Int](children) }

}
// ------------------------------------------------------------------------------

def parseTree(input: String): Tree[Int] = {
  import TreeParser.*
  TreeParser.parse(tree, input) match {
    case Success(result, next) => result
    case failure: NoSuccess    => throw RuntimeException(failure.msg)
  }
}

def parse(input: String): List[List[Tree[Int]]] =
  input.trim
    .split("\n\n")
    .toList
    .map { block =>
      block.split("\n", 2) match {
        case Array(first, second) => List(parseTree(first), parseTree(second))
      }
    }

// ------------------------------------------------------------------------------

def treeDiffing1(a: Tree[Int], b: Tree[Int]): Int =
  (a, b) match {
    case (Leaf(ai), Leaf(bi))                       => ai.compare(bi)
    case (Node(ac), Node(bc)) if ac.size == bc.size => ac.zip(bc).map(treeDiffing1).find(_ != 0).getOrElse(0)
    case (Node(ac), Node(bc)) if ac.size < bc.size  => ac.zip(bc).map(treeDiffing1).find(_ != 0).getOrElse(-1)
    case (Node(ac), Node(bc)) if ac.size > bc.size  => ac.zip(bc).map(treeDiffing1).find(_ != 0).getOrElse(1)
    case (a: Leaf[Int], b: Node[Int])               => treeDiffing1(Node(a :: Nil), b)
    case (a: Node[Int], b: Leaf[Int])               => treeDiffing1(a, Node(b :: Nil))
  }

def resolveStar1(input: String): Int =
  val treePairList = parse(input)
  treePairList
    .collect { case List(a, b) => treeDiffing1(a, b) }
    .zipWithIndex
    .collect { case (result, index) if result <= 0 => index + 1 }
    .sum

// ------------------------------------------------------------------------------

def resolveStar2(input: String): Int =
  val trees    = parse(input).flatten
  val divider1 = parseTree("[[2]]")
  val divider2 = parseTree("[[6]]")
  val result   = (divider1 :: divider2 :: trees).sortWith((a, b) => treeDiffing1(a, b) == -1)
  (result.indexOf(divider1) + 1) * (result.indexOf(divider2) + 1)

// ------------------------------------------------------------------------------

object Puzzle13Test extends ZIOSpecDefault {
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
        exampleResult == 13,
        puzzleResult == 6076
      )
    },
    test("star#2") {
      for {
        exampleInput1 <- fileContent(Path(s"data/$day/example-1.txt"))
        exampleResult1 = resolveStar2(exampleInput1)
        puzzleInput   <- fileContent(Path(s"data/$day/puzzle-1.txt"))
        puzzleResult   = resolveStar2(puzzleInput)
      } yield assertTrue(
        exampleResult1 == 140,
        puzzleResult == 24805
      )
    }
  ) @@ timed @@ sequential
}
