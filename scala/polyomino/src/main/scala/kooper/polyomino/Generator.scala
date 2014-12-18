package kooper.polyomino

import scala.io.StdIn

object Generator {
  
  def generate(nBlocks: Int): Set[Polyomino] = {
    if(nBlocks == 1) 
      Set(Polyomino((0, 0)))
    else    
      generate(nBlocks - 1).map(generateAllOfPlusOneBlock(_)).flatten
  }
  
  private def generateByAddingBlocksAround(polyomino: Polyomino, point: Point) = {
    val adjacentPointDeltas = Set((-1, 0), (0, -1), (1, 0), (0, 1))
    
    for {
      (dx, dy) <- adjacentPointDeltas
      newPoint = point.move(dx, dy)
      if(!polyomino.contains(newPoint))
    } yield (polyomino + newPoint).normalize
  }
  
  private def generateAllOfPlusOneBlock(polyomino: Polyomino) = {
    polyomino.points.toSet
      .map((p: Point) => generateByAddingBlocksAround(polyomino, p))
      .flatten
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
      polyominos.foreach(print)
  }
}  

