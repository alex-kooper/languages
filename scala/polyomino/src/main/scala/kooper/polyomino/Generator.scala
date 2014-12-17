package kooper.polyomino

object Generator {
  
  def generate(length: Int): Set[Polyomino] = {
    if(length == 1) 
      Set(Polyomino((0, 0)))
    else    
      generate(length - 1).map(generateAllOfPlusOneLength(_)).flatten
  }
  
  private def generateByAddingPointsAround(polyomino: Polyomino, point: Point) = {
    val adjacentPointDeltas = Set((-1, 0), (0, -1), (1, 0), (0, 1))
    
    for {
      (dx, dy) <- adjacentPointDeltas
      newPoint = point.move(dx, dy)
      if(!polyomino.contains(newPoint))
    } yield (polyomino + newPoint).normalize
  }
  
  private def generateAllOfPlusOneLength(polyomino: Polyomino) = {
    var result:Set[Polyomino] = Set()
    
    for(p <- polyomino.points)
      result = result ++ generateByAddingPointsAround(polyomino, p)
    
    result
  }

  def main(args: Array[String]) {
    println(generate(5))
  }
}  

