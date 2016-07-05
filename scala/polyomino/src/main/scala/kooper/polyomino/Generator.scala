package kooper.polyomino

import scala.io.StdIn

object Generator {
  
  def generate(nPoints: Int): Set[Polyomino] = {
    if(nPoints == 1) 
      Set(Polyomino((0, 0)))
    else    
      generate(nPoints - 1).par.flatMap(generateByAddingOnePoint(_)).seq
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
  
  def formatDuration(d: Long) = {
    val durationInSeconds = d / 1000000000.0
    val hours = (durationInSeconds / 3600).toInt
    val hoursRem = durationInSeconds % 3600
    val minutes = (hoursRem / 60).toInt
    val seconds = hoursRem % 60
    
    "%d:%02d:%f".format(hours, minutes, seconds)
  }
  
  def main(args: Array[String]) {
    print("Enter number of cells: ")
    val n = StdIn.readInt
    
    val start = System.nanoTime()
    val polyominos = generate(n)
    val end = System.nanoTime()
    val duration = end - start
    
    println("There are " + polyominos.size + " free polyominoes with " + 
            n + " cells.")
    
    println("It took " + formatDuration(duration) +  " seconds to generate them.")
    
    print("Would you like to see all of them? [y/n]: ")
    val yesOrNo = StdIn.readChar()
    
    if(yesOrNo.toLower == 'y')
      polyominos.foreach(p => print(p.render))
  }
}  

