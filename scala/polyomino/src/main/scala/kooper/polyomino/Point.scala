package kooper.polyomino

case class Point(val x: Int, val y: Int) extends Ordered[Point] {
  
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
  
  override def compare(that: Point) = {
    val compareX = this.x compare that.x
    
    if(compareX == 0) 
      this.y compare that.y 
    else 
      compareX
  }
}

