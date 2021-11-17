// Trees: Level order traversal of binary tree

// Given the root of a binary tree, display the node values at each level.
// Node values for all levels should be displayed on separate lines.

enum BTree[+T]:
  case Node(value: T, left: BTree[T], right: BTree[T])
  case Leaf

  def isLeaf: Boolean = this match
    case Leaf => true
    case _    => false

import BTree.{Node, Leaf}

// mkNode only necessary because Node constructor does not work with
// default parameter values. I suspect it is a bug in Scala3.
def mkNode[T](value: T, left: BTree[T] = Leaf, right: BTree[T] = Leaf) =
  BTree.Node(value, left, right)

def nextLevel[T](level: Seq[BTree[T]]) =
  level
    .flatMap { t =>
      t match
        case Node(value, left, right) => Seq(left, right)
        case Leaf                     => Seq()
    }
    .filter(!_.isLeaf)

def levelOrderTraversal[T](root: BTree[T]) =
  LazyList.iterate(Seq(root))(nextLevel).takeWhile(_.nonEmpty)

def nodeToString[T](n: BTree[T]) = n match
  case Leaf              => ""
  case Node(value, _, _) => value.toString

def levelOrderTraversalToString[T](root: BTree[T]) =
  levelOrderTraversal(root)
    .map { level =>
      level.map(nodeToString).mkString(", ")
    }
    .mkString("\n")

@main def printLevelOrderTraversal =
  val tree = mkNode(
    100,
    mkNode(
      50,
      mkNode(25),
      mkNode(75)
    ),
    mkNode(
      200,
      Leaf,
      mkNode(350)
    )
  )

  println(levelOrderTraversalToString(tree))
