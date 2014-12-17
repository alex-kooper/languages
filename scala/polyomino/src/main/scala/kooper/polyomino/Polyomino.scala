package kooper.polyomino

case class Polyomino(val points: Set[Point]) {
  
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
  def rotateLeft(p: Point) = new Polyomino(points.map(_.rotateLeft(p)))
  
  def normalize = {
    val Point(x, y) = upperLeftCorner
    this.move(-x, -y)
  }
  
  override def toString = {
    val n = this.normalize
    
    var a = Array.tabulate(height, width)((y, x) => 
      if(n.points.contains(Point(x, y))) "[]" else "  "
    )
    
    a.map(_.mkString).mkString("\n", "\n", "\n")
  }
}

object Polyomino {
  def apply(xs: (Int, Int)*) = {
    val points = for((x,y) <-xs) yield Point(x, y)
    new Polyomino(points.toSet)  
  }
}
