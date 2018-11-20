import Data.List
import Data.List.Split

type Digit = Int
type Row = Int
type Column = Int
type Cell = (Row, Column)

-- Grid is represented as list in row-major order
newtype Grid = Grid [Digit] deriving (Show)

readGrid :: String -> Grid
readGrid = Grid . map toInt . splitOn ","
  where
    toInt s = read s :: Int

getCell :: Grid -> Cell -> Digit
getCell (Grid digits) (row, col) = digits !! (row * 9 + col)

rowCells :: Int -> [Cell]
rowCells row = [(row, c) | c <- [0..8]]

columnCells :: Int -> [Cell]
columnCells column = [(r, column) | r <- [0..8]]

-- Subgrids are indexed like cells but the valid range for
-- rows & colums is [0..2]
subgridCells :: Cell -> [Cell]
subgridCells (row, column) = [(r, c) | r <- [subgridFirst row .. subgridLast row],
                                       c <- [subgridFirst column .. subgridLast column]]
  where
    subgridFirst x = x * 3
    subgridLast x = subgridFirst x + 3 - 1

validateCells :: Grid -> [Cell] -> Bool
validateCells grid cells = length (nub values) == 9
  where
    values = fmap (grid `getCell`) cells

isValidRow :: Grid -> Int -> Bool
isValidRow grid row = validateCells grid $ rowCells row

isValidColumn :: Grid -> Int -> Bool
isValidColumn grid row = validateCells grid $ rowCells row

isValidSubgrid :: Grid -> (Int, Int) -> Bool
isValidSubgrid grid (row, col) = validateCells grid $ subgridCells (row, col)

isValidGrid :: Grid -> Bool
isValidGrid grid = rowsAreValid && columnsAreValid && subgridsAreValid
  where
    rowsAreValid = all (isValidRow grid) [0..8]
    columnsAreValid = all (isValidColumn grid) [0..8]
    subgridsAreValid = all (isValidSubgrid grid) [(r, c) | r <- [0..2], c <- [0..2]]

gridStr :: String
gridStr = "1,2,7,5,3,9,8,4,6,4,5,3,8,6,1,7,9,2,8,9,6,4,7,2,1,5,3,2,8,9,3,1,7,4,6,5,3,6,5,2,8,4,9,1,7,7,4,1,9,5,6,3,2,8,9,7,4,6,2,8,5,3,1,5,1,2,7,4,3,6,8,9,6,3,8,1,9,5,2,7,4"

invalidGrid :: String
invalidGrid =
  "1,2,3,4,5,6,7,8,9,\
  \2,3,4,5,6,7,8,9,1,\
  \3,4,5,6,7,8,9,1,2,\
  \4,5,6,7,8,9,1,2,3,\
  \5,6,7,8,9,1,2,3,4,\
  \6,7,8,9,1,2,3,4,5,\
  \7,8,9,1,2,3,4,5,6,\
  \8,9,1,2,3,4,5,6,7,\
  \9,1,2,3,4,5,6,7,8"

validGrid :: String
validGrid =
  "1,2,7,5,3,9,8,4,6,\
  \4,5,3,8,6,1,7,9,2,\
  \8,9,6,4,7,2,1,5,3,\
  \2,8,9,3,1,7,4,6,5,\
  \3,6,5,2,8,4,9,1,7,\
  \7,4,1,9,5,6,3,2,8,\
  \9,7,4,6,2,8,5,3,1,\
  \5,1,2,7,4,3,6,8,9,\
  \6,3,8,1,9,5,2,7,4"

main :: IO ()
main = do
  putStrLn $ "This grid should be invalid: " ++ show (isValidGrid $ readGrid invalidGrid)
  putStrLn $ "This grid should be valid: " ++ show (isValidGrid $ readGrid validGrid)
