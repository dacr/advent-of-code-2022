package day07

import zio.*
import zio.test.*
import zio.test.TestAspect.*

// ------------------------------------------------------------------------------
sealed trait Tree[K, T] {
  val key: K
}

case class Leaf[K, T](key: K, content: T) extends Tree[K, T]

case class Node[K, T](key: K, children: Set[Tree[K, T]] = Set.empty) extends Tree[K, T] {
  def isLeaf = children.isEmpty
}

/** Build a tree from a list of tuple made of path and content. The path must contain both the node keys and the leaf key.
  *
  * @param pathContentTuples
  *   the list of path/content tuple - <b>all path must at least contain one key</b>
  * @param rootKey
  *   the key used for the tree root
  * @tparam K
  *   the data type used for node key
  * @tparam T
  *   the data type used for content
  * @return
  *   the built tree
  * @author
  *   David Crosson
  */
def pathsToTree[K, T](pathContentTuples: List[(List[K], T)], rootKey: K): Tree[K, T] = {
  def worker(PathContentTuples: List[(List[K], T)], currentKey: K): Tree[K, T] = {
    PathContentTuples.partition { case (path, _) => path.size == 1 } match {
      case (leafPaths, nodePaths) =>
        val nodeChildren: List[Tree[K, T]] =
          nodePaths
            .groupBy { case (path, _) => path.head }
            .map { case (key, subpaths) => key -> subpaths.map { case (path, item) => path.tail -> item } }
            .map { case (key, subpaths) => worker(subpaths, key) }
            .toList
        val leafChildren: List[Tree[K, T]] = leafPaths.collect { case (key :: Nil, item) => Leaf(key, item) }
        Node(currentKey, children = nodeChildren.toSet ++ leafChildren)
    }
  }
  worker(pathContentTuples, rootKey)
}

/** Build the list of all possible path and content tuples from a tree
  *
  * @param tree
  *   the tree to walk though in order to be build possible paths
  * @param ignoreRootKey
  *   do not insert the root key on generated paths
  * @tparam K
  *   the data type used for node key
  * @tparam T
  *   the data type used for content
  * @return
  *   the built list of path/content tuples
  * @author
  *   David Crosson
  */
def treeToPaths[K, T](tree: Tree[K, T], ignoreRootKey: Boolean = true): List[(List[K], T)] = {
  def worker(subtrees: List[Tree[K, T]], currentPath: List[K]): List[(List[K], T)] = {
    subtrees.flatMap {
      case Leaf(key, content)  => List((key :: currentPath).reverse -> content)
      case Node(key, children) => worker(children.toList, key :: currentPath)
    }
  }
  tree match {
    case Leaf(key, content)  => List((key :: Nil) -> content)
    case Node(key, children) => worker(children.toList, if (ignoreRootKey) Nil else key :: Nil)
  }
}

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
    case Some(itemFileRE(size, name))   => buildPaths(instructions.tail, currentPath, ((name :: currentPath) -> File(name, size.toInt)) :: filesByPath)
    // case Some(itemDirRE(name))          => buildPaths(instructions.tail, currentPath, ((name :: currentPath) -> Directory(name)) :: filesByPath)
    case Some(itemDirRE(name))          => buildPaths(instructions.tail, currentPath, filesByPath)
    case _                              => filesByPath.map { case (path, item) => path.reverse -> item }
  }
}

def parse(input: List[String]) =
  val paths = buildPaths(input, Nil, Nil)
  val tree  = pathsToTree(paths, "root")
  tree

// ------------------------------------------------------------------------------

def du(tree: Tree[String, Item]): Long = {
  tree match {
    case Node(name, children)       => children.map(du).sum
    case Leaf(name, file: File)     => file.size
    case Leaf(name, dir: Directory) => 0L // TODO
  }
}

def searchCappedSize(tree: Tree[String, Item]): List[Long] = {
  tree match {
    case Leaf(name, file: File)     => Nil
    case Leaf(name, dir: Directory) => Nil // TODO
    case Node(name, children)       =>
      val currentSize = du(tree)
      if (currentSize > 100000L) children.toList.flatMap(searchCappedSize)
      else currentSize :: children.toList.flatMap(searchCappedSize)
  }
}

def resolveStar1(input: List[String]): Long =
  val tree = parse(input)
  searchCappedSize(tree).sum

// ------------------------------------------------------------------------------

def dirSizes(tree: Tree[String, Item]): List[(String, Long)] = {
  tree match {
    case Leaf(name, file: File)     => Nil
    case Leaf(name, dir: Directory) => Nil // TODO
    case Node(name, children)       =>
      (name -> du(tree)) :: children.toList.flatMap(dirSizes)
  }
}
def resolveStar2(input: List[String]): Long                  =
  val tree   = parse(input)
  val sizes  = dirSizes(tree)
  val duRoot = du(tree)
  sizes
    .filter((name, size) => 70_000_000L - duRoot + size >= 30_000_000L)
    .map { case (name, size) => size }
    .min

// ------------------------------------------------------------------------------

object Puzzle07Test extends ZIOSpecDefault {
  import zio.nio.file.Path
  import helpers.Helpers.*
  val day  = getClass.getName.replaceAll(""".*Puzzle(\d+)Test.*""", "day$1")
  def spec = suite(s"puzzle $day")(
    suite("paths and trees test")(
      // -------------------------------------------------
      test("just a leaf") {
        val paths = List((1 :: Nil) -> "A")
        val tree  = Node(0, Set(Leaf(1, "A")))
        assertTrue(
          pathsToTree(paths, 0) == tree,
          treeToPaths(tree) == paths
        )
      },
      // -------------------------------------------------
      test("one path to tree") {
        val paths = List(
          (1 :: 1 :: 1 :: Nil) -> "A"
        )
        val tree  = Node(0, Set(Node(1, Set(Node(1, Set(Leaf(1, "A")))))))
        assertTrue(
          pathsToTree(paths, 0) == tree,
          treeToPaths(tree) == paths
        )
      },
      // -------------------------------------------------
      test("fixed size paths to tree") {
        val paths = List(
          (1 :: 1 :: Nil) -> "A",
          (1 :: 2 :: Nil) -> "B"
        )
        val tree  = Node("root", Set(Node(1, Set(Leaf(1, "A"), Leaf(2, "B")))))
        assertTrue(
          pathsToTree(paths, "root") == tree,
          treeToPaths(tree) == paths
        )
      }
    ),
    // ======================================================
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
  ) @@ timed @@ sequential
}
