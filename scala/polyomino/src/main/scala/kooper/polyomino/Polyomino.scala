package kooper.polyomino

import collection.immutable.SortedSet

case class Polyomino(val points: SortedSet[Point]) extends Ordered[Polyomino] {
  
  def +(p: Point) = new Polyomino(points + p)
  
  def contains(p: Point) = points.contains(p) 
  
  def upperLeftCorner = {
    val x = points.map(_.x).min	
    val y = points.map(_.y).min
    new Point(x, y)
  }

  def lowerRightCorner = {
    val x = points.map(_.x).max	
    val y = points.map(_.y).max
    new Point(x, y)
  }
  
  def width = lowerRightCorner.x - upperLeftCorner.x + 1
  def height = lowerRightCorner.y - upperLeftCorner.y + 1
  
  def move(dx: Int, dy: Int) = new Polyomino(points.map(_.move(dx, dy)))
  
  def rotateRight(p: Point) = new Polyomino(points.map(_.rotateRight(p)))
  def rotateRight: Polyomino = this rotateRight Point.origin
  
  def rotateLeft(p: Point) = new Polyomino(points.map(_.rotateLeft(p)))
  def rotateLeft: Polyomino = this rotateLeft Point.origin
  
  def moveToOrigin = {
    val Point(x, y) = upperLeftCorner
    this.move(-x, -y)
  }
  
  override def toString = {
    val n = this.moveToOrigin
    
    var a = Array.tabulate(height, width)((y, x) => 
      if(n.points.contains(Point(x, y))) "[]" else "  "
    )
    
    a.map(_.mkString).mkString("\n", "\n", "\n")
  }
  
  override def compare(that: Polyomino) = {
    import Ordering.Implicits._
    Ordering[List[Point]].compare(this.points.toList, that.points.toList)
  }
  
  def allRotations = {
    var rotations = List(this)
    var p = this
    
    for(i <- 1 to 3) {
      p = p.rotateRight.moveToOrigin
      rotations = p :: rotations
    }
    
    rotations
  }
  
  def normalize = this.allRotations.filter(p => p.width >= p.height).max
}

object Polyomino {
  def apply(xs: (Int, Int)*) = {
    val points = for((x,y) <-xs) yield Point(x, y)
    new Polyomino(points.to[SortedSet])  
  }
}
