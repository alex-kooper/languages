import Control.Monad
import System.Environment

type QueenRow = Int
newtype Board = Board [QueenRow] deriving (Show)

emptyBoard :: Board
emptyBoard = Board []

addQueenInRow :: Board -> QueenRow -> Board
(Board rows) `addQueenInRow` row = Board $ row : rows

underAttack :: QueenRow -> Board -> Bool
underAttack row (Board rows) = rowHasQueens ||
                               upDiagonalHasQueens ||
                               downDiagonalHasQueens
  where
    rowHasQueens = row `elem` rows

    upDiagonalHasQueens = or $ zipWith (==) upDiagonal rows
    upDiagonal = iterate (+1) $ row + 1

    downDiagonalHasQueens = or $ zipWith (==) downDiagonal rows
    downDiagonal = iterate (subtract 1) $ row - 1

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
