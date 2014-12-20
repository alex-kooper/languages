package kooper.polyomino

import collection.immutable.SortedSet

class Polyomino(val points: SortedSet[Point]) extends Ordered[Polyomino] {
  
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
  
  def reflectVertically(x: Int) = new Polyomino(points.map(_.reflectVertically(x)))
  def reflectOverTheYAxis = reflectVertically(0)

  def reflectHorizontally(y: Int) = new Polyomino(points.map(_.reflectHorizontally(y)))
  def reflectOverTheXAxis = reflectHorizontally(0)
  
  def moveToOrigin = {
    val Point(x, y) = upperLeftCorner
    this.move(-x, -y)
  }
  
  def allRotations = {
    var rotations = Set(this.moveToOrigin)
    var p = this
    
    for(i <- 1 to 3) {
      p = p.rotateRight.moveToOrigin
      rotations += p
    }
    
    rotations
  }
  
  def allCongruents = allRotations ++ reflectOverTheXAxis.allRotations
  
  def normalize = this.allCongruents.filter(p => p.width >= p.height).max
  
  def render = {
    val p = this.moveToOrigin
    
    var matrix = Array.tabulate(height, width)(
      (y, x) => 
        if(p.points.contains(Point(x, y))) "[]" else "  "
    )
    
    matrix.map(_.mkString).mkString("\n", "\n", "\n")
  }
  
  override def toString = {
    var Point(x, y) = upperLeftCorner
    "Position: " + (x, y) + render + "\n"
  }
  
  override def equals(other: Any) = other match {
    case that: Polyomino => 
      (that canEqual this) &&
      this.points == that.points
    
    case _ => false
  }
  
  def canEqual(other: Any) = other.isInstanceOf[Polyomino]
  
  override def hashCode = this.points.hashCode
  
  override def compare(that: Polyomino) = {
    import Ordering.Implicits._
    Ordering[List[Point]].compare(this.points.toList, that.points.toList)
  }
}

object Polyomino {
  def apply(xs: (Int, Int)*) = {
    val points = for((x,y) <-xs) yield Point(x, y)
    new Polyomino(points.to[SortedSet])  
  }
}
