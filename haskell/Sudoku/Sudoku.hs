
import Data.Char
import Data.Maybe
import Data.Foldable

import qualified Data.Map as Map
import Data.Map (Map)

import qualified Data.Set as Set
import Data.Set (Set, union, (\\))

import Control.Monad
import System.Environment
import Text.Printf

type Digit = Int
type Row = Int
type Column = Int
type Cell = (Row, Column)

newtype Grid = Grid (Map Cell Digit)

type CellConstraints = Map Cell (Set Digit)

addCell :: Grid -> Cell -> Digit -> Grid
addCell (Grid gridMap) cell digit = Grid $ Map.insert cell digit gridMap

getCell :: Grid -> Cell -> Maybe Digit
getCell (Grid gridMap) cell = Map.lookup cell gridMap

unknownCells :: Grid -> [Cell]
unknownCells grid = [cell | (cell, Nothing) <- cellsWithValues]
  where
    cellsWithValues = [((r, c), grid `getCell` (r, c)) | r <- [1..9], c <- [1..9]]

initialCellConstraints :: Grid -> CellConstraints
initialCellConstraints grid = Map.fromList [(c, cellConstraint c) | c <- unknownCells grid]
  where
    cellConstraint cell = Set.fromList [1..9] \\ relatedCellValues cell
    relatedCellValues = Set.fromList .
                        catMaybes .
                        fmap (grid `getCell`) .
                        toList .
                        relatedCells

relatedCells :: Cell -> Set Cell
relatedCells (row, column) = rowCells `union` columnCells `union` subgridCells
  where
    rowCells = Set.fromList [(row, c) | c <- [1..9]]
    columnCells = Set.fromList [(r, column) | r <- [1..9]]

    subgridCells = Set.fromList [(r, c) | r <- [subgridFirst row .. subgridLast row],
                                          c <- [subgridFirst column .. subgridLast column]]

    subgridFirst x = (x - 1) `div` 3 * 3 + 1
    subgridLast x = subgridFirst x + 3 - 1

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
  [ renderRow 1, separator1, renderRow 2, separator1, renderRow 3, separator2
  , renderRow 4, separator1, renderRow 5, separator1, renderRow 6, separator2
  , renderRow 7, separator1, renderRow 8, separator1, renderRow 9]
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
