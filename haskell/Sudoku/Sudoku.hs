
import Data.Char
import Data.Maybe
import qualified Data.Map as Map
import Data.Map (Map)
import Control.Monad
import System.Environment
import Text.Printf

type Digit = Int
type Row = Int
type Column = Int
type Cell = (Row, Column)

newtype Grid = Grid (Map Cell Digit)

addCell :: Grid -> Cell -> Digit -> Grid
addCell (Grid gridMap) cell digit = Grid $ Map.insert cell digit gridMap

getCell :: Grid -> Cell -> Maybe Digit
getCell (Grid gridMap) cell = Map.lookup cell gridMap

parseGrid :: String -> Grid
parseGrid s = Grid $ Map.fromList cellsWithDigits
  where
    filterLine = filter (\c -> isDigit c || c == '.')
    filteredLines = filter (not . null) $ map filterLine $ lines s

    cellsWithDigits = do
      (rowNumber, row) <- zip [1..] filteredLines
      (columnNumber, char) <- zip [1..] row

      guard $ isDigit char
      let digit = read [char] :: Int

      return ((rowNumber, columnNumber), digit)

renderGrid :: Grid -> String
renderGrid grid = unlines
  [ renderRow 1, separator1
  , renderRow 2, separator1
  , renderRow 3, separator2
  , renderRow 4, separator1
  , renderRow 5, separator1
  , renderRow 6, separator2
  , renderRow 7, separator1
  , renderRow 8, separator1
  , renderRow 9 ]
  where
    renderRow row =
      printf "%s  %s  %s | %s  %s  %s | %s  %s  %s"
             (c 1) (c 2) (c 3) (c 4) (c 5) (c 6) (c 7) (c 8) (c 9)
      where
        c i = fromMaybe "." $ show <$> grid `getCell` (row, i)

    separator1 = "        |         |        "
    separator2 = "--------+---------+--------"

main :: IO ()
main = do
  [fileName] <- getArgs
  contents <- readFile fileName
  putStrLn $ renderGrid $ parseGrid contents
