import Data.Set hiding (filter, take)
import qualified Data.Set as Set
import Data.List(intercalate)

import Control.Monad
import Control.Arrow

data Cell = Cell { cellX :: Int, cellY :: Int } deriving (Show, Read, Eq, Ord)

data Grid = Grid
  { width :: Int
  , height :: Int
  , aliveCells :: Set Cell }
  deriving (Show)

mkEmptyGrid :: Int -> Int -> Grid
mkEmptyGrid w h = Grid w h Set.empty

fitIntoGrid :: Cell -> Grid -> Cell
(Cell x y) `fitIntoGrid` (Grid w h _) = Cell (x `mod` w) (y `mod` h)

isAliveIn :: Cell -> Grid -> Bool
cell `isAliveIn` grid = c `member` aliveCells grid
  where
    c = cell `fitIntoGrid` grid

setAlive :: Grid -> [Cell] -> Grid
setAlive grid @ (Grid w h cells) cellsToSet = Grid w h $
  cells `union` Set.fromList cellsToSet'
  where
    cellsToSet' = (`fitIntoGrid` grid) <$> cellsToSet

setDead :: Grid -> [Cell] -> Grid
setDead grid @ (Grid w h cells) cellsToSet = Grid w h $
  cells `difference` Set.fromList cellsToSet'
  where
    cellsToSet' = (`fitIntoGrid` grid) <$> cellsToSet

rangeOfX :: Grid -> [Int]
rangeOfX grid = [0 .. width grid - 1]

rangeOfY :: Grid -> [Int]
rangeOfY grid = [0 .. height grid - 1]

parseTextPicture :: String -> Grid
parseTextPicture picture = mkEmptyGrid width' height' `setAlive` aliveCells'
  where
    lines' = lines picture
    height' = length lines'
    width' = maximum $ length <$> lines'

    aliveCells' = do
      (y, line) <- zip [0..] lines'
      (x, char) <- zip [0..] line
      guard $ char == '*'
      return $ Cell x y

renderTextPicture :: Grid -> String
renderTextPicture grid = intercalate "\n" $ renderLine <$> rangeOfY grid
  where
    renderCell cell = if cell `isAliveIn` grid then '*' else '.'
    renderLine y = (\x -> renderCell $ Cell x y) <$> rangeOfX grid

allNeighbours :: Cell -> [Cell]
allNeighbours (Cell x y) = [Cell (x + dx) (y + dy) |
                            dx <- [-1..1], dy <- [-1..1],
                            dx /= 0 || dy /= 0 ]

countAliveNeighboursIn :: Cell -> Grid -> Int
cell `countAliveNeighboursIn` grid =
  length $ filter (`isAliveIn` grid) $ allNeighbours cell

nextGeneration :: Grid -> Grid
nextGeneration grid @ (Grid w h _) = mkEmptyGrid w h `setAlive` aliveInNextGeneration
  where
    aliveInNextGeneration = do
      x <- rangeOfX grid
      y <- rangeOfY grid

      let cell = Cell x y
          aliveNeighbours = cell `countAliveNeighboursIn` grid

      guard $ aliveNeighbours > 1
      guard $ aliveNeighbours < 4
      guard $ aliveNeighbours == 3 || cell `isAliveIn` grid

      return cell

generations :: Grid -> [Grid]
generations = iterate nextGeneration

picture1 :: String
picture1 =
  ".....\n\
  \.....\n\
  \.***.\n\
  \.....\n\
  \.....\n"

main :: IO()
main = do
  let pipeline =
        parseTextPicture >>>
        generations >>>
        take 7 >>>
        fmap renderTextPicture

  mapM_ putStrLn $ pipeline picture1
