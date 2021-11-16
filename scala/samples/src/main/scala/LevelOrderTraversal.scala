// Trees: Level order traversal of binary tree

// Given the root of a binary tree, display the node values at each level. 
// Node values for all levels should be displayed on separate lines. 

enum BinaryTree[+T]:
  case Node(value: T, left: BinaryTree[T] = Leaf, right: BinaryTree[T] = Leaf)
  case Leaf

  def isLeaf: Boolean = this match
    case Leaf => true
    case _    => false

import BinaryTree.*

def nextLevel[T](level: Seq[BinaryTree[T]]) =
  level
    .flatMap { t =>
      t match
        case Node(value, left, right) => Seq(left, right)
        case Leaf                     => Seq()
    }
    .filter(!_.isLeaf)

def levelOrderTraversal[T](root: BinaryTree[T]) =
  LazyList.iterate(Seq(root))(nextLevel).takeWhile(_.nonEmpty)

def nodeToString[T](n: BinaryTree[T]) = n match
  case Leaf              => ""
  case Node(value, _, _) => value.toString

def levelOrderTraversalToString[T](root: BinaryTree[T]) =
  levelOrderTraversal(root)
    .map { level =>
      level.map(nodeToString).mkString(", ")
    }
    .mkString("\n")

@main def printLevelOrderTraversal =
  val tree = Node(
    100,
    Node(
      50,
      new Node(25, Leaf, Leaf),
      Node(75, Leaf, Leaf)
    ),
    Node(
      200,
      Leaf,
      Node(350, Leaf, Leaf)
    )
  )
  println(levelOrderTraversalToString(tree))
