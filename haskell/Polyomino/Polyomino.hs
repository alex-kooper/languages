module Polyomino (
    Polyomino(..),
    belongsTo,
    add,
    upperLeftCorner,
    lowerRightCorner,
    width,
    height,
    move,
    rotateRight,
    rotateLeft,
    moveToOrigin,
    Polyomino.fromList,
    renderPolyomino,
    normalize
) where

import Prelude hiding (map)
import qualified Prelude(map)
import Data.List(intercalate)
import Data.Set

import Point(Point(..))
import qualified Point

data Polyomino = Polyomino { points :: Set Point } deriving (Eq, Ord)

belongsTo :: Point -> Polyomino -> Bool
point `belongsTo` (Polyomino points) = point `member` points

add :: Point -> Polyomino -> Polyomino
add point (Polyomino points) = Polyomino $ insert point points

upperLeftCorner :: Polyomino -> Point
upperLeftCorner (Polyomino points) = Point x y
    where
        x = findMin $ map Point.x points
        y = findMin $ map Point.y points 

lowerRightCorner:: Polyomino -> Point
lowerRightCorner (Polyomino points) = Point x y
    where
        x = findMax $ map Point.x points
        y = findMax $ map Point.y points 

width :: Polyomino -> Int
width p = (Point.x $ lowerRightCorner p) - (Point.x $ upperLeftCorner p) + 1

height :: Polyomino -> Int
height p = (Point.y $ lowerRightCorner p) - (Point.y $ upperLeftCorner p) + 1

move :: Int -> Int -> Polyomino -> Polyomino
move dx dy (Polyomino points) = Polyomino $ map (Point.move dx dy) points

rotateLeft :: Point -> Polyomino -> Polyomino
rotateLeft point (Polyomino points) = Polyomino $ map (Point.rotateLeft point) points

rotateRight :: Point -> Polyomino -> Polyomino
rotateRight point (Polyomino points) = Polyomino $ map (Point.rotateRight point) points

moveToOrigin :: Polyomino -> Polyomino
moveToOrigin p = move (-x) (-y) p
    where Point x y = upperLeftCorner p
 
fromList :: [(Int, Int)] -> Polyomino
fromList xs = Polyomino $ Data.Set.fromList $ Prelude.map toPoint xs 
    where toPoint (x, y) = Point x y

renderPoint :: Point -> Polyomino -> String
renderPoint point polyomino = if point `belongsTo` polyomino then "[]" else "  "  

renderLine :: [Int] -> Int -> Polyomino -> String
renderLine xRange y polyomino = 
    concat [renderPoint (Point x y) polyomino | x <- xRange]

renderRectangle :: [Int] -> [Int] -> Polyomino -> String
renderRectangle xRange yRange polyomino = 
    intercalate "\n" [renderLine xRange y polyomino | y <- yRange] 

renderPolyomino :: Polyomino -> String 
renderPolyomino p = "\n" ++ (renderRectangle [x1..x2] [y1..y2] polyomino) ++ "\n"
    where
       polyomino = moveToOrigin p
       Point x1 y1 = upperLeftCorner polyomino 
       Point x2 y2 = lowerRightCorner polyomino

allRotations :: Polyomino -> Set Polyomino
allRotations p = rotations 3 p (singleton $ moveToOrigin p)
    where
        rotations 0 p xs = xs

        rotations n p xs = rotations (n - 1) newP (insert newP xs)
            where newP = rotate p

        rotate = moveToOrigin . (rotateRight $ Point 0 0)

normalize :: Polyomino -> Polyomino
normalize = findMax . Data.Set.filter isWide . allRotations
    where
        isWide p = width p >= height p

instance Show Polyomino where
    show = renderPolyomino

tetramino :: Polyomino
tetramino = Polyomino.fromList [(1, 0), (2, 0), (3, 0), (3, 1)]


