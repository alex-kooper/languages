package kooper.polyomino

class Point(val x: Int, val y: Int) {
  
  def move(dx: Int, dy: Int) = new Point(x + dx, y + dy)
  
  def rotateRight(p: Point) = {
    val newX = -(this.y - p.y) + p.x
    val newY = (this.x - p.x) + p.y;
    new Point(newX, newY);   
  } 
    
  def rotateLeft(p: Point) = {
     val newX = (this.y - p.y) + p.x
     val newY = -(this.x - p.x) + p.y
     new Point(newX, newY);   
  }
  
  override def toString = "Point(" + this.x + ", " + this.y + ")"
}
