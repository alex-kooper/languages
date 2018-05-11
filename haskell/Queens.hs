import Control.Monad
import System.Environment
import Text.Printf

type QueenRow = Int
newtype Board = Board [QueenRow]

emptyBoard :: Board
emptyBoard = Board []

addQueenInRow :: Board -> QueenRow -> Board
(Board rows) `addQueenInRow` row = Board $ row : rows

renderBoard :: Board -> String
renderBoard (Board rows) =
  unlines [renderRow n | n <- [minimum rows .. maximum rows]]
  where
    renderRow row = concatMap (renderCell row) rows
    renderCell renderedRow row = if renderedRow == row then "Q " else ". "

instance Show Board where
  show = renderBoard

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
solutions _ 0 = [emptyBoard]

solutions nRows nCols = do
  board <- solutions nRows (nCols - 1)
  row <- [1 .. nRows]
  guard $ not $ underAttack row board
  return $ board `addQueenInRow` row

main :: IO ()
main = do
  [nCols, nRows] <- map (read :: String -> Int) <$> getArgs

  let s = solutions nCols nRows
      n = length s

  printf "There are %d solutions for the %dx%d board.\n" n nRows nCols

  when (n > 0) $ do
    putStrLn ""
    putStrLn "Here is one of them"
    putStrLn ""
    print $ head s
