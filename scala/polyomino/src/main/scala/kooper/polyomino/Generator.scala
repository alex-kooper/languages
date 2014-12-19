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
    print("Enter number of blocks: ")
    val n = StdIn.readInt
    
    val polyominos = generate(n)
    
    println("\nThere are " + polyominos.size + " shapes " +
            "that can be constructed out of " + n + " blocks")
    
    print("Do you want me to show them? [y/n]: ")
    val yesOrNo = StdIn.readChar()
    
    if(yesOrNo.toLower == 'y')
      polyominos.foreach(p => print(p.render))
  }
}  

