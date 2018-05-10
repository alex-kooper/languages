import Control.Monad
import System.Environment

type QueenRow = Int
newtype Board = Board [QueenRow] deriving (Show)

emptyBoard :: Board
emptyBoard = Board []

addQueenInRow :: Board -> QueenRow -> Board
(Board rows) `addQueenInRow` row = Board $ row : rows

rowHasQueens :: QueenRow -> Board -> Bool
rowHasQueens row (Board rows) = row `elem` rows

upDiagonalHasQueens :: QueenRow -> Board -> Bool
upDiagonalHasQueens row (Board rows) = or $ zipWith (==) diagonal rows
  where
    diagonal = iterate (+1) $ row + 1

downDiagonalHasQueens :: QueenRow -> Board -> Bool
downDiagonalHasQueens row (Board rows) = or $ zipWith (==) diagonal rows
  where
    diagonal = iterate (subtract 1) $ row - 1

underAttack :: QueenRow -> Board -> Bool
underAttack row board = rowHasQueens row board ||
                        upDiagonalHasQueens row board ||
                        downDiagonalHasQueens row board

solutions :: Int -> Int -> [Board]
solutions 0 _ = [emptyBoard]

solutions nCols nRows = do
  board <- solutions (nCols - 1) nRows
  row <- [1 .. nRows]
  guard $ not $ underAttack row board
  return $ board `addQueenInRow` row

main :: IO ()
main = do
  [nCols, nRows] <- map (read :: String -> Int) <$> getArgs
  putStrLn $ "Number of solutions: " ++ show (length $ solutions nCols nRows)
  
