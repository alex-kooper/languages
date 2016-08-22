import Data.List
import Data.Maybe
import Data.Function
import qualified Data.Map as Map
import Text.Printf
import System.IO
import System.Environment

type Square = (Int, Int)
type SquareValue = Int
 
data Board = Board { 
    width          :: Int,
    height         :: Int,
    squares        :: Map.Map Square SquareValue,
    lastMove       :: Maybe Square,
    lastMoveNumber :: Int,
    nVisited       :: Int
}

makeBoard :: Int -> Int -> Board
makeBoard w h = Board { 
   width = w, 
   height = h, 
   squares = Map.empty,
   lastMove = Nothing,
   lastMoveNumber = 0,
   nVisited = 0
}

inBoard :: Board -> Square -> Bool
inBoard board (x,y) = 
    1 <= x && x <= (width board) &&
    1 <= y && y <= (height board)

isVisited :: Board -> Square -> Bool
isVisited board square = isJust $ Map.lookup square (squares board)

allVisited :: Board -> Bool
allVisited board = nVisited board == width board * height board

makeMoveTo :: Board -> Square -> Board
makeMoveTo board square = Board {
        width = width board,
        height = height board,
        squares = Map.insert square nextMoveNumber (squares board),
        lastMove = Just square,
        lastMoveNumber = nextMoveNumber,
        nVisited = (nVisited board) + 1
    }
    where nextMoveNumber = (lastMoveNumber board) + 1

renderBoard :: Board -> String
renderBoard board = 
    intercalate "\n" [
        intercalate "" [
            renderSquare (c, r) |
            c <- [1..(width board)]
        ] |
        r <- (reverse [1..(height board)])
    ] 
    where renderSquare square = 
              let longestNumber = length $ show $ width board * height board
                  s = Map.lookup square (squares board) 
              in case s of 
                  (Just n) -> printf ("%" ++ (show $ longestNumber + 1) ++ "d") n 
                  (Nothing) -> replicate 2 ' ' ++ "*" 

instance Show Board where
    show = renderBoard

movesFrom :: Square -> [Square]
movesFrom (x, y) = [(x + dx, y + dy) | 
                    dx <- [-2..2], dy <- [-2..2],
                    dx /= 0, dy /= 0, abs dx /= abs dy]

movesOnBoardFrom :: Board -> Square -> [Square]
movesOnBoardFrom board square = filter isValid $ movesFrom square
    where isValid = (&&) <$> inBoard board <*> (not . isVisited board)

solveStartingFrom :: Board -> Maybe(Board)
solveStartingFrom board
    | allVisited board = Just board
    | otherwise =
        let square = fromJust $ lastMove board 
            squares = movesOnBoardFrom board square
            nMoves = length . movesOnBoardFrom board
            squares' = sortBy (compare `on` nMoves) squares
            boards = map (makeMoveTo board) squares'
            candidates = map solveStartingFrom boards
        in fromJust <$> find isJust candidates 

solve :: Int -> Int -> Maybe(Board)
solve n m =
    let board = makeBoard n m
        boards = [makeMoveTo board (x, y) | x <- [1..n], y <- [1..m]]
        candidates = map solveStartingFrom boards
    in fromJust <$> find isJust candidates

main = do 
    [n, m] <- map (read :: String -> Int) <$> getArgs
    let solution = solve n m
    case solution of
        (Just board) -> print board
        Nothing -> putStrLn "No solution!"

