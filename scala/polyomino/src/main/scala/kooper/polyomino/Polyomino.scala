package kooper.polyomino

case class Polyomino(val points: List[Point]) {
  
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
  
  def move(dx: Int, dy: Int) = points.map(_.move(dx, dy))
  def rotateRight(p: Point) = points.map(_.rotateRight(p))
  def rotateLeft(p: Point) = points.map(_.rotateLeft(p))
  
  def normalize = {
    val Point(x, y) = upperLeftCorner
    this.move(-x, -y)
  }  
}
