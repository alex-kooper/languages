module Main where

import Prelude

import Data.Maybe(Maybe(Just, Nothing), isNothing, fromMaybe)
import Data.Array (length, zip, (..), filter, null, catMaybes, head)

import Data.Map (Map)
import Data.Map as Map

import Data.Set (Set, union, difference, size)
import Data.Set as Set

import Data.Char.Unicode (isDigit)
import Control.MonadZero (guard)
import Data.Tuple (Tuple(..))
import Data.Int (fromString)
import Data.String.Utils (lines)
import Data.String.CodeUnits (singleton, toCharArray)
import Data.Foldable (intercalate, foldl, minimumBy)

import Effect (Effect)
import Effect.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

type Digit = Int
type Row = Int
type Column = Int
type Cell = { row :: Row, column :: Column }

newtype Grid = Grid (Map Cell Digit)

type CellConstraints = Map Cell (Set Digit)

addCell :: Grid -> Cell -> Digit -> Grid
addCell (Grid gridMap) cell digit = Grid $ Map.insert cell digit gridMap

getCell :: Grid -> Cell -> Maybe Digit
getCell (Grid gridMap) cell = Map.lookup cell gridMap

unknownCells :: Grid -> Array Cell
unknownCells grid = do
  row <- 1 .. 9
  column <- 1 .. 9
  
  let cell = { row, column }
  guard $ isNothing $ grid `getCell` cell
  pure cell


relatedCells :: Cell -> Set Cell
relatedCells { row, column } = rowCells `union` columnCells `union` subgridCells
  where
    rowCells = Set.fromFoldable $ map { row, column: _ } (1 .. 9)
    columnCells = Set.fromFoldable $ map { row: _, column } (1 .. 9)

    subgridCells = Set.fromFoldable $ do
      r <- subgridFirst row .. subgridLast row
      c <- subgridFirst column .. subgridLast column
      pure { row: r, column: c }

    subgridFirst x = (x - 1) `div` 3 * 3 + 1
    subgridLast x = subgridFirst x + 3 - 1

initialCellConstraints :: Grid -> CellConstraints
initialCellConstraints grid  = 
  unknownCells
  >>> map (\c -> (Tuple c (cellConstraint c)))
  >>> Map.fromFoldable
  $ grid 
  
  where
    cellConstraint cell = Set.fromFoldable (1 .. 9) `difference` relatedCellValues cell
    relatedCellValues = 
      relatedCells
      >>> Set.toUnfoldable
      >>> map (grid `getCell` _)
      >>> catMaybes
      >>> Set.fromFoldable


-- Propagate constraint for all the unknown cells after fixing the value
-- of one unknown cell (row, column) to value digit
fixCellValue :: CellConstraints -> Cell -> Digit -> CellConstraints
fixCellValue constraints { row, column } digit = Map.delete { row, column } adjustConstraints
  where
    adjustConstraint cs { row: r, column: c } = 
      Map.update (Just <<< (Set.delete digit)) { row: r, column: c } cs
    
    adjustConstraints = foldl adjustConstraint constraints $ relatedCells { row, column }


mostConstraintedCell :: CellConstraints -> Maybe (Tuple Cell (Set Digit))
mostConstraintedCell = minimumBy (comparing numberOfDigits) <<< toArray
  where
    numberOfDigits (Tuple _ values) = size values
    
    toArray :: CellConstraints -> Array (Tuple Cell (Set Digit))
    toArray = Map.toUnfoldable


solve :: Grid -> Array Grid
solve unsolvedGrid = findSoutions unsolvedGrid $ initialCellConstraints unsolvedGrid
  where
    findSoutions :: Grid -> CellConstraints -> Array Grid
    findSoutions grid constraints = case mostConstraintedCell constraints of
      Nothing -> [grid]
      Just (Tuple cell values) -> do
        value <- Set.toUnfoldable values

        let constraints' = fixCellValue constraints cell value
            grid' = addCell grid cell value

        findSoutions grid' constraints'    


parseGrid :: String -> Grid
parseGrid s = Grid $ Map.fromFoldable cellsWithDigits
  where
    filterLine = filter (\c -> isDigit c || c == '.')
    
    filteredLines = 
      lines
      >>> map(toCharArray >>> filterLine)
      >>> filter (not <<< null)
      $ s    
    
    cellsWithDigits = do
      (Tuple row rowNumber) <- zipWithIndex filteredLines
      (Tuple char columnNumber) <- zipWithIndex row
      guard $ isDigit char
      
      let digit = fromMaybe 0 $ fromString $ singleton char
      
      pure $ Tuple { row: rowNumber, column: columnNumber } digit

    zipWithIndex :: forall a. Array a -> Array (Tuple a Int)
    zipWithIndex xs = zip xs $ 1 .. length xs


renderGrid :: Grid -> String
renderGrid grid = intercalate "\n"
  [ renderRow 1, separator1, renderRow 2, separator1, renderRow 3, separator2
  , renderRow 4, separator1, renderRow 5, separator1, renderRow 6, separator2
  , renderRow 7, separator1, renderRow 8, separator1, renderRow 9]
  where
    renderRow row = 
      (c 1) <> "  " <> (c 2) <> "  " <> (c 3) <> " | " <>
      (c 4) <> "  " <> (c 5) <> "  " <> (c 6) <> " | " <>
      (c 7) <> "  " <> (c 8) <> "  " <> (c 9)

      where
        c i = fromMaybe "." $ show <$> grid `getCell` { row, column: i }

    separator1 = "        |         |        "
    separator2 = "--------+---------+--------"


main :: Effect Unit
main = 
  readTextFile UTF8 "puzzles/puzzle2.txt"
  >>= pure <<< process 
  >>= log
  where
    process = 
      parseGrid 
      >>> solve
      >>> head 
      >>> map renderGrid 
      >>> fromMaybe "There is no solution to this Sudoku puzzle"
