import qualified Data.Set as Set
import Data.List

import Polyomino
import qualified Point

adjacentPointDeltas = [(-1, 0), (0, -1), (1, 0), (0, 1)]

generateBasedOnPoint :: Polyomino -> Point.Point -> [Polyomino]
generateBasedOnPoint polyomino point = [normalize $ add newPoint polyomino | 
                                        (dx, dy) <- adjacentPointDeltas, 
                                        let newPoint = Point.move dx dy point,
                                        not (newPoint `belongsTo` polyomino)]

generateBasedOn :: Polyomino -> [Polyomino] 
generateBasedOn polyomino = concat [generateBasedOnPoint polyomino p |
                                    p <- Set.toList $ points polyomino]

generatePolyominos :: Int -> [Polyomino]

generatePolyominos 1 = [fromList [(0, 0)]]

generatePolyominos n = 
    nub $ concat [generateBasedOn p | p <- generatePolyominos (n - 1)]

