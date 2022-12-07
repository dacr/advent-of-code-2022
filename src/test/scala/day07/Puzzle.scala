package day07

import zio.*
import zio.test.*

// ------------------------------------------------------------------------------
sealed trait Tree[T]
case class Leaf[T](content: T)                            extends Tree[T]
case class Node[T](name: String, children: List[Tree[T]]) extends Tree[T]

// ------------------------------------------------------------------------------
sealed trait Item
case class File(name: String, size: Long) extends Item
case class Directory(name: String)        extends Item

type Path = List[String]

// ------------------------------------------------------------------------------
val commandRootDirRE   = """^[$] cd /""".r
val commandParentDirRE = """^[$] cd [.][.]""".r
val commandChangeDirRE = """^[$] cd (.+)""".r
val commandListRE      = """^[$] ls""".r

val itemFileRE = """^(\d+) (.+)$""".r
val itemDirRE  = """^dir (.+)""".r

@annotation.tailrec
def buildPaths(instructions: List[String], currentPath: Path, filesByPath: List[(Path, Item)]): List[(Path, Item)] = {
  instructions.headOption match {
    case Some(commandRootDirRE())       => buildPaths(instructions.tail, Nil, filesByPath)
    case Some(commandParentDirRE())     => buildPaths(instructions.tail, currentPath.tail, filesByPath)
    case Some(commandChangeDirRE(name)) => buildPaths(instructions.tail, name :: currentPath, filesByPath)
    case Some(commandListRE())          => buildPaths(instructions.tail, currentPath, filesByPath)
    case Some(itemFileRE(size, name))   => buildPaths(instructions.tail, currentPath, (currentPath -> File(name, size.toInt)) :: filesByPath)
    case Some(itemDirRE(name))          => buildPaths(instructions.tail, currentPath, (currentPath -> Directory(name)) :: filesByPath)
    case None                           => filesByPath.map { case (path, item) => path.reverse -> item }
  }
}

def pathsToTree[T](currentName: Option[String], paths: List[(Path, T)]): Tree[T] = {
  paths.partition { case (path, _) => path.isEmpty } match {
    case (emptyPaths, otherPaths) =>
      val nodeChildren: List[Tree[T]] =
        otherPaths
          .groupBy { case (path, _) => path.head }
          .map { case (key, subpaths) => key -> subpaths.map { case (path, item) => path.tail -> item } }
          .map { case (key, subpaths) => pathsToTree(Some(key), subpaths) }
          .toList
      val leafChildren: List[Tree[T]] = emptyPaths.map { case (_, item) => Leaf(item) }
      Node(currentName.getOrElse("ROOT"), children = nodeChildren ::: leafChildren)
  }
}

def parse(input: List[String]) =
  val paths = buildPaths(input, Nil, Nil)
  val tree  = pathsToTree(None, paths)
  tree

// ------------------------------------------------------------------------------

def du(tree: Tree[Item]): Long = {
  tree match {
    case Node(name, children) => children.map(du).sum
    case Leaf(file: File)     => file.size
    case Leaf(dir: Directory) => 0L // TODO
  }
}

def searchCappedSize(tree: Tree[Item]): List[Long] = {
  tree match {
    case Leaf(file: File)     => Nil
    case Leaf(dir: Directory) => Nil // TODO
    case Node(name, children) =>
      val currentSize = du(tree)
      if (currentSize > 100000L) children.flatMap(searchCappedSize)
      else currentSize :: children.flatMap(searchCappedSize)
  }
}

def resolveStar1(input: List[String]): Long =
  val tree = parse(input)
  searchCappedSize(tree).sum

// ------------------------------------------------------------------------------

def dirSizes(tree: Tree[Item]): List[(String, Long)] = {
  tree match {
    case Leaf(file: File)     => Nil
    case Leaf(dir: Directory) => Nil // TODO
    case Node(name, children) =>
      (name -> du(tree)) :: children.flatMap(dirSizes)
  }
}
def resolveStar2(input: List[String]): Long          =
  val tree  = parse(input)
  val sizes = dirSizes(tree)
  val duRoot = du(tree)
  sizes
    .filter((name, size) => 70_000_000L-duRoot+size >= 30_000_000L)
    .map{case (name, size) => size}
    .min



// ------------------------------------------------------------------------------

object Puzzle07Test extends ZIOSpecDefault {
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
        exampleResult == 95437L,
        puzzleResult == 1581595L
      )
    },
    test("star#2") {
      for {
        exampleInput1 <- fileLines(Path(s"data/$day/example-1.txt"))
        exampleResult1 = resolveStar2(exampleInput1)
        puzzleInput   <- fileLines(Path(s"data/$day/puzzle-1.txt"))
        puzzleResult   = resolveStar2(puzzleInput)
      } yield assertTrue(
        exampleResult1 == 24933642L,
        puzzleResult == 1544176L
      )
    }
  )
}
