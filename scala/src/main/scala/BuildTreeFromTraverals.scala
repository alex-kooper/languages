// This was asked during Jet.com coding interview
//
// Construct Tree from given Inorder and Preorder traversals
//  Let us consider the below traversals:
//
//  Inorder sequence: D B E A F C
//  Preorder sequence: A B D E C F
//

sealed trait Tree[+T]

case class Node[+T](value: T, left: Tree[T] = Leaf, right: Tree[T] = Leaf) extends Tree[T] {
  override def toString: String = (left, right) match {
    case (Leaf, Leaf) => s"$value"
    case (_, _) => s"$value($left, $right)"
  }
}

case object Leaf extends Tree[Nothing] {
  override def toString = "()"
}

object BuildTreeFromTraverals extends App {

  protected def lazyBuildTree[T](preOrder: Seq[T], inOrder: Seq[T]): (Tree[T], Seq[T]) =
    if(inOrder.isEmpty) (Leaf, preOrder) else {
      val root = preOrder.head
      val (left, rightWithRoot) = inOrder.span(_ != root)
      val right = rightWithRoot.tail

      val (leftTree, newPreOrder1) = lazyBuildTree(preOrder.tail, left)
      val (rightTree, newPreOrder2) = lazyBuildTree(newPreOrder1, right)

      (Node(root, leftTree, rightTree), newPreOrder2)
  }

  def buildTree[T](preOrder: Seq[T], inOrder: Seq[T]) = lazyBuildTree(preOrder.view, inOrder.view)._1
}

object BuildTreeFromTraversalsTests extends App {
  import BuildTreeFromTraverals.buildTree

  def inOrderTraversal[T](t: Tree[T]): Stream[T] = t match {
    case Leaf => Stream.empty
    case Node(value, left, right) => inOrderTraversal(left) #::: value #:: inOrderTraversal(right)
  }

  def preOrderTraversal[T](t: Tree[T]): Stream[T] = t match {
    case Leaf => Stream.empty
    case Node(value, left, right) => value #:: preOrderTraversal(left) #::: preOrderTraversal(right)
  }

  val tree =
    Node(
      5,
      Node(
        2,
        Node(1),
        Node(
          3,
          Leaf,
          Node(4)
        )
      ),
      Node(
        7,
        Node(6),
        Node(
          8,
          Leaf,
          Node(9)
        )
      )
    )

  println(s"Original tree: $tree")
  println(s"Rebuilt tree: ${buildTree(preOrderTraversal(tree), inOrderTraversal(tree))}")

  val preOrder = Seq('A', 'B', 'D', 'E', 'C', 'F')
  val inOrder = Seq('D', 'B', 'E', 'A', 'F', 'C')

  println(s"Example tree: ${buildTree(preOrder, inOrder)}")
}
