import Data.Set hiding (map, fromList)
import qualified Data.Set as Set
import Control.Applicative
import Data.Char
import System.IO

import Polyomino hiding (fromList)
import qualified Polyomino
import qualified Point

generateByAddingOnePoint :: Polyomino -> Set Polyomino
generateByAddingOnePoint polyomino = 
    Set.fromList
        [normalize $ add newPoint polyomino |
            p <- toList $ points polyomino,
            (dx, dy) <- adjacentPointDeltas, 
            let newPoint = Point.move dx dy p,
            not (newPoint `belongsTo` polyomino)]
    where
        adjacentPointDeltas = [(-1, 0), (0, -1), (1, 0), (0, 1)]

generate:: Int -> Set Polyomino

generate 1 = singleton $ Polyomino.fromList [(0, 0)]

generate n = 
    unions $ map generateByAddingOnePoint (toList $ generate (n - 1))

main = do
    putStr "Enter number of cells: "
    hFlush stdout
    n <- (read <$> getLine)

    let polyominoes = generate n
    let nPolyominoes = show $ size polyominoes

    putStrLn $ "There are " ++ nPolyominoes ++ 
               " free polyominoes with " ++ (show n) ++ " cells"

    putStr "Would you like to see all of them? [y/n]: " 
    hFlush stdout
    yesOrNo <- getLine

    if (toLower $ head yesOrNo) == 'y' then
        putStrLn $ concatMap renderPolyomino (toList polyominoes)
    else
        return ()


