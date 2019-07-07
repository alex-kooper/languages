module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)

import Data.Maybe(Maybe, isNothing, fromMaybe)
import Data.Array (length, zip, (..), filter, null)

import Data.Map (Map)
import Data.Map as Map

import Data.Set (Set, union)
import Data.Set as Set

import Data.Char.Unicode (isDigit)
import Control.MonadZero (guard)
import Data.Tuple (Tuple(..))
import Data.Int (fromString)
import Data.String.Utils (lines)
import Data.String.CodeUnits (singleton, toCharArray)
import Data.Foldable (intercalate)

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


puzzle :: String
puzzle = """
.  .  4 | 8  .  . | .  1  7
        |         |        
6  7  . | 9  .  . | .  .  .
        |         |        
5  .  8 | .  3  . | .  .  4
--------+---------+--------
3  .  . | 7  4  . | 1  .  .
        |         |        
.  6  9 | .  .  . | 7  8  .
        |         |        
.  .  1 | .  6  9 | .  .  5
--------+---------+--------
1  .  . | .  8  . | 3  .  6
        |         |        
.  .  . | .  .  6 | .  9  1
        |         |        
2  4  . | .  .  1 | 5  .  .
"""

main :: Effect Unit
main = do
  log $ renderGrid $ parseGrid puzzle
