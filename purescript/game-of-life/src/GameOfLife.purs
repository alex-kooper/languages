module Main where

import Prelude

import Data.Array ((..), zip, filter)
import Data.Array as A
import Data.Foldable (intercalate, maximum)
import Data.Maybe (fromMaybe)
import Data.Set (Set, member, union)
import Data.Set as Set
import Data.String.CodeUnits as S
import Data.String.Utils (lines)
import Data.Tuple (Tuple(..))
import Data.List.Lazy (List, iterate, (!!))

import Control.MonadZero (guard)
import Effect (Effect)
import Effect.Console (log)

type Cell = { x :: Int, y :: Int }

type Grid = 
  { width :: Int
  , height :: Int 
  , aliveCells :: Set Cell }

mkEmptyGrid :: Int -> Int -> Grid
mkEmptyGrid w h = { width: w, height: h, aliveCells: Set.empty }

fitIntoGrid :: Cell -> Grid -> Cell
fitIntoGrid { x,  y }  { width, height } = 
  { x: (x `mod` width)
  , y: (y `mod` height) }

isAliveIn :: Cell -> Grid -> Boolean
isAliveIn cell grid = c `member` grid.aliveCells
  where
    c = cell `fitIntoGrid` grid

setAlive :: Grid -> Array Cell -> Grid
setAlive grid @ { width, height, aliveCells } cellsToSet = 
  grid { aliveCells = aliveCells' }
  where
    aliveCells' = aliveCells `union` Set.fromFoldable cellsToSet'
    cellsToSet' = (_ `fitIntoGrid` grid) <$> cellsToSet    

rangeOfX :: Grid -> Array Int
rangeOfX grid = 0 .. (grid.width - 1)

rangeOfY :: Grid -> Array Int
rangeOfY grid = 0 .. (grid.height - 1)

parseTextPicture :: String -> Grid
parseTextPicture picture = mkEmptyGrid width' height' `setAlive` aliveCells'
  where
    lines' = lines picture
    height' = A.length lines'
    width' = fromMaybe 0 $ maximum $ S.length <$> lines'

    aliveCells' = do
      (Tuple line y) <- zipWithIndex lines'
      (Tuple char x) <- zipWithIndex $ S.toCharArray line
      guard $ char == '*'
      pure $ { x, y }
    
    zipWithIndex :: forall a. Array a -> Array (Tuple a Int)
    zipWithIndex xs = zip xs $ 1 .. A.length xs

renderTextPicture :: Grid -> String
renderTextPicture grid = intercalate "\n" $ renderLine <$> rangeOfY grid
  where
    renderCell cell = if cell `isAliveIn` grid then '*' else '.'
    renderLine y = S.fromCharArray $ (\x -> renderCell $ { x, y }) <$> rangeOfX grid

allNeighbours :: Cell -> Array Cell
allNeighbours { x, y } = do
  dx <- -1 .. 1
  dy <- -1 .. 1
  guard $ dx /= 0 || dy /= 0
  pure { x: x + dx, y: y + dy }

countAliveNeighboursIn :: Cell -> Grid -> Int
countAliveNeighboursIn cell grid =
  A.length $ filter (_ `isAliveIn` grid) $ allNeighbours cell

nextGeneration :: Grid -> Grid
nextGeneration grid @ { width, height } = 
  mkEmptyGrid width height `setAlive` aliveInNextGeneration
  where
    aliveInNextGeneration = do
      x <- rangeOfX grid
      y <- rangeOfY grid

      let cell = { x, y }
          aliveNeighbours = cell `countAliveNeighboursIn` grid

      guard $ aliveNeighbours > 1
      guard $ aliveNeighbours < 4
      guard $ aliveNeighbours == 3 || cell `isAliveIn` grid

      pure cell

generations :: Grid -> List Grid
generations = iterate nextGeneration

example :: String
example = """
..................
..................
..................
..................
........*.........
.........*........
.......***........
..................
..................
..................
..................
"""

main :: Effect Unit
main = do
  log "Game of Life, step 10"  
  log $ 
    parseTextPicture 
    >>> generations 
    >>> (_ !! 10)
    >>> map renderTextPicture
    >>> fromMaybe ""
    $ example
  