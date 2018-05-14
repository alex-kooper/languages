
import Data.Char
import Data.Maybe
import qualified Data.Map as Map
import Data.Map (Map)
import Control.Monad
import System.Environment

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
renderGrid grid = unlines [renderRow row | row <- [1..9]]
  where
    renderRow row = concat [renderCell row col | col <- [1..9]]
    renderCell row col = fromMaybe " . " $ showDigit <$> grid `getCell` (row, col)
    showDigit d = [' ', head $ show d, ' ']

main :: IO ()
main = do
  [fileName] <- getArgs
  contents <- readFile fileName
  putStrLn $ renderGrid $ parseGrid contents
