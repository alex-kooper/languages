package kooper.polyomino

class Point(val x: Int, y: Int) {
  def move(dx: Int, dy: Int) = new Point(x + dx, y + dy)
}