import scala.annotation.tailrec

/**
  * Mishura's Binary Search Tree lazy traversal.
  */
object BSTreeTraversal  extends App {

  sealed trait BSTree[+A]

  case object BSTNil extends BSTree[Nothing]
  case class BSTNode[+A](a: A, left: BSTree[A] = BSTNil, right: BSTree[A] = BSTNil) extends BSTree[A]


  /*
                5
                |
           -----------------
           |                |
           2                7
           |                |
        ----------        --------
        |         |       |       |
        1         3       6       8
                  |               |
                 -----           -----
                 |    |          |    |
                 *    4          *    9

   */


  val t3 = BSTNode(3, BSTNil, BSTNode(4))
  val t2 = BSTNode(2, BSTNode(1), t3)
  val t8 = BSTNode(8, BSTNil, BSTNode(9))
  val t7 = BSTNode(7, BSTNode(6), t8)
  val tree = BSTNode(5, t2, t7)

  def traverse[A](t: BSTree[A], acc: Stream[A] = Stream.Empty): Stream[A] = t match {
    case BSTNil => acc

    case BSTNode(a, left, right) =>
      println(s"node $a")
      val rs = a #::traverse(right, acc)
      traverse(left, rs)
  }

  val ss = traverse(tree).take(4)
  println("lazy stream created")
  println(ss.toList)

}



