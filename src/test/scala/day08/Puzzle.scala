package day08

import zio.*
import zio.test.*

case class Tree(height: Int) extends AnyVal
case class Forest(trees: Chunk[Chunk[Tree]]) {
  val width                                 = trees.size
  val height                                = trees.headOption.map(_.size).getOrElse(0)
  def getTree(y: Int, x: Int): Option[Tree] = trees.lift(y).flatMap(_.lift(x)) // TODO BAD
  def isBorder(y: Int, x: Int): Boolean     = {
    y == 0 || y == (height - 1) || x == 0 || x == (width - 1)
  }
}

// ------------------------------------------------------------------------------
def parse(input: List[String]) =
  val trees = Chunk.from(
    input.map(line => Chunk.from(line.map(ch => Tree(ch - '0'))))
  )
  Forest(trees)

def computeVisibilityMatrix(forest: Forest): Chunk[Chunk[Boolean]] = {
  forest.trees.zipWithIndex.map { (treesLine, y) =>
    treesLine.zipWithIndex.map { (tree, x) =>
      forest.isBorder(y, x) ||
      0.until(x).forall(xc => forest.getTree(y, xc).map(_.height).filter(_ < tree.height).isDefined) ||
      (x + 1).until(forest.width).forall(xc => forest.getTree(y, xc).map(_.height).filter(_ < tree.height).isDefined) ||
      0.until(y).forall(yc => forest.getTree(yc, x).map(_.height).filter(_ < tree.height).isDefined) ||
      (y + 1).until(forest.height).forall(yc => forest.getTree(yc, x).map(_.height).filter(_ < tree.height).isDefined)
    }
  }
}

// ------------------------------------------------------------------------------

def resolveStar1(input: List[String]): Int =
  val forest       = parse(input)
  val visibilities = computeVisibilityMatrix(forest)
  visibilities.flatten.count(_ == true)

// ------------------------------------------------------------------------------
//// for posterity - what horrible piece of code !!!
//def countVisibleBabyCode(referenceHeight: Int, heights: Iterable[Int]): Int = {
//  var count    = 0
//  val it       = heights.iterator
//  var continue = true
//  while (it.hasNext && continue) {
//    val current = it.next
//    if (current < referenceHeight) count += 1
//    else if (current >= referenceHeight) {
//      count += 1
//      continue = false
//    } else continue = false
//  }
//  count
//}

def countVisible(referenceHeight: Int, heights: LazyList[Int]): Int =
  heights.indexWhere(_ >= referenceHeight) match {
    case -1 => heights.size
    case i  => i + 1
  }

def computeScenicScores(forest: Forest): Chunk[Chunk[Int]] = {
  forest.trees.zipWithIndex.map { (treesLine, y) =>
    treesLine.zipWithIndex.map { (tree, x) =>
      if (forest.isBorder(y, x)) 0
      else {
        val northHeights = LazyList.from((y - 1).to(0, -1)).flatMap(yc => forest.getTree(yc, x).map(_.height))
        val leftHeights  = LazyList.from((x - 1).to(0, -1)).flatMap(xc => forest.getTree(y, xc).map(_.height))
        val southHeights = LazyList.from((y + 1).until(forest.height)).flatMap(yc => forest.getTree(yc, x).map(_.height))
        val rightHeights = LazyList.from((x + 1).until(forest.width)).flatMap(xc => forest.getTree(y, xc).map(_.height))

        List(northHeights, leftHeights, southHeights, rightHeights)
          .map(heights => countVisible(tree.height, heights))
          .product
      }
    }
  }
}

def resolveStar2(input: List[String]): Int =
  val forest = parse(input)
  val scores = computeScenicScores(forest)
  scores.flatten.max

// ------------------------------------------------------------------------------

object Puzzle08Test extends ZIOSpecDefault {
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
        exampleResult == 21,
        puzzleResult == 1816
      )
    },
    test("star#2") {
      for {
        exampleInput1 <- fileLines(Path(s"data/$day/example-1.txt"))
        exampleResult1 = resolveStar2(exampleInput1)
        puzzleInput   <- fileLines(Path(s"data/$day/puzzle-1.txt"))
        puzzleResult   = resolveStar2(puzzleInput)
      } yield assertTrue(
        exampleResult1 == 8,
        puzzleResult == 383520
      )
    }
  )
}
