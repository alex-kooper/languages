import Data.Set hiding (filter)
import qualified Data.Set as Set
import Data.List(intercalate)
import Control.Monad
import Control.Arrow

data Cell = Cell { cellX :: Int, cellY :: Int } deriving (Show, Read, Eq, Ord)
newtype Grid = Grid(Set Cell) deriving (Show)

emptyGrid :: Grid
emptyGrid = Grid Set.empty

isAliveIn :: Cell -> Grid -> Bool
cell `isAliveIn` (Grid cells) = cell `member` cells

setAlive :: Grid -> [Cell] -> Grid
setAlive (Grid cells) cellsToSet =
  Grid $ cells `union` Set.fromList cellsToSet

setDead :: Grid -> [Cell] -> Grid
setDead (Grid cells) cellsToSet =
  Grid $ cells `difference` Set.fromList cellsToSet

range :: Grid -> (Cell -> Int) -> [Int]
range (Grid cells) fn
  | Set.null cells = []
  | otherwise       = [minValue .. maxValue]
  where
    s = Set.map fn cells
    minValue = findMin s - 1
    maxValue = findMax s + 1

rangeX :: Grid -> [Int]
rangeX grid = grid `range` cellX

rangeY :: Grid -> [Int]
rangeY grid = grid `range` cellY

parseTextPicture :: String -> Grid
parseTextPicture picture = emptyGrid `setAlive` aliveCells
  where
    aliveCells = do
      (y, line) <- zip [0..] $ lines picture
      (x, char) <- zip [0..] line
      guard $ char == '*'
      return $ Cell x y

renderTextPicture :: Grid -> String
renderTextPicture grid = intercalate "\n" $ renderLine <$> rangeY grid
  where
    renderCell cell = if cell `isAliveIn` grid then '*' else '.'
    renderLine y = (\x -> renderCell $ Cell x y) <$> rangeX grid

allNeighbours :: Cell -> [Cell]
allNeighbours (Cell x y) = [Cell (x + dx) (y + dy) |
                            dx <- [-1..1], dy <- [-1..1],
                            dx /= 0 || dy /= 0 ]

countAliveNeighboursIn :: Cell -> Grid -> Int
cell `countAliveNeighboursIn` grid =
  length $ filter (`isAliveIn` grid) $ allNeighbours cell

nextGeneration :: Grid -> Grid
nextGeneration grid = emptyGrid `setAlive` aliveCells
  where
    aliveCells = do
      x <- rangeX grid
      y <- rangeY grid

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
