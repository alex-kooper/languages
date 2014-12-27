import qualified Data.Set as Set
import Data.List

import Polyomino
import qualified Point

generateByAddingOnePoint :: Polyomino -> [Polyomino]
generateByAddingOnePoint polyomino = [normalize $ add newPoint polyomino |
                                      p <- Set.toList $ points polyomino,
                                      (dx, dy) <- adjacentPointDeltas, 
                                      let newPoint = Point.move dx dy p,
                                      not (newPoint `belongsTo` polyomino)]
    where
        adjacentPointDeltas = [(-1, 0), (0, -1), (1, 0), (0, 1)]

generatePolyominos :: Int -> [Polyomino]

generatePolyominos 1 = [fromList [(0, 0)]]

generatePolyominos n = 
    nub $ concat [generateByAddingOnePoint p | p <- generatePolyominos (n - 1)]

