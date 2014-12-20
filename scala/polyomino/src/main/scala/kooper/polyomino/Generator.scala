package kooper.polyomino

import scala.io.StdIn

object Generator {
  
  def generate(nPoints: Int): Set[Polyomino] = {
    if(nPoints == 1) 
      Set(Polyomino((0, 0)))
    else    
      generate(nPoints - 1).flatMap(generateByAddingOnePoint(_))
  }
  
  private def generateByAddingOnePoint(polyomino: Polyomino): Set[Polyomino] = {
    val adjacentPointDeltas = Set((-1, 0), (0, -1), (1, 0), (0, 1))
    
    for {
      p <- polyomino.points
      (dx, dy) <- adjacentPointDeltas
      newPoint = p.move(dx, dy)
      if(!polyomino.contains(newPoint))
    } yield (polyomino + newPoint).normalize
  }
  
  def main(args: Array[String]) {
    print("Enter number of cells: ")
    val n = StdIn.readInt
    
    val polyominos = generate(n)
    
    println("\nThere are " + polyominos.size + " free polyominoes with " + 
            n + " cells.")
    
    print("Do you want me to show them? [y/n]: ")
    val yesOrNo = StdIn.readChar()
    
    if(yesOrNo.toLower == 'y')
      polyominos.foreach(p => print(p.render))
  }
}  

