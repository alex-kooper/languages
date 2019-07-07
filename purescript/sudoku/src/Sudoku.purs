module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)

import Data.Maybe(Maybe, isNothing)
import Data.Array((..), filter, zip, length)

import Data.Map (Map)
import Data.Map as Map

import Data.Set (Set, union)
import Data.Set as Set

import Data.Char.Unicode (isDigit)
import Data.String.Utils (lines)
import Control.MonadZero (guard)
import Data.Tuple
import Data.Int (fromString)
import Data.String (singleton)
import Data.String.Utils (lines, toCharArray)

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

{-
parseGrid :: String -> Grid
parseGrid s = Grid $ Map.fromFoldable cellsWithDigits
  where
    filterLine :: Array Char -> Array Char
    filterLine = filter (\c -> isDigit c || c == '.')
    filteredLines = map (toCharArray >>> filterLine) $ lines s

    cellsWithDigits = do
      (Tuple row rowNumber) <- zipWithIndex filteredLines
      (Tuple char columnNumber) <- zipWithIndex $ row

      guard $ isDigit char
      let digit = fromString $ singleton char

      pure Tuple { row: rowNumber, column: columnNumber } digit
    
    zipWithIndex xs = zip xs (1 .. length xs)
-}

main :: Effect Unit
main = do
  log "Hello, World!"
