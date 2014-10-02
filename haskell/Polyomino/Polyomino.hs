module Polyomino (
    Polyomino(..),
    Dimensions(..),
    upperLeftCorner,
    getDimensions,
    move,
    rotateRight,
    rotateLeft,
    normalize,
    Polyomino.fromList,
    belongsTo,
    add,
    renderPolyomino    
) where

import Prelude hiding (map)
import qualified Prelude(map)
import Data.List(intercalate)
import Data.Set

import Point(Point(..))
import qualified Point

data Polyomino = Polyomino { points :: Set Point } 

data Dimensions = Dimensions { 
    xRange :: [Int], 
    yRange :: [Int] 
} deriving (Show, Eq)

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
  
getDimensions :: Polyomino -> Dimensions
getDimensions (Polyomino points) = Dimensions [x1..x2] [y1..y2] 
    where
        x1 = findMin $ map Point.x points
        x2 = findMax $ map Point.x points
        y1 = findMin $ map Point.y points 
        y2 = findMax $ map Point.y points

move :: Int -> Int -> Polyomino -> Polyomino
move dx dy (Polyomino points) = Polyomino $ map (Point.move dx dy) points

rotateLeft :: Point -> Polyomino -> Polyomino
rotateLeft point (Polyomino points) = Polyomino $ map (Point.rotateLeft point) points

rotateRight :: Point -> Polyomino -> Polyomino
rotateRight point (Polyomino points) = Polyomino $ map (Point.rotateRight point) points

normalize :: Polyomino -> Polyomino
normalize p = move (-x) (-y) p
    where Point x y = upperLeftCorner p
 
fromList :: [(Int, Int)] -> Polyomino
fromList xs = Polyomino $ Data.Set.fromList $ Prelude.map toPoint xs 
    where toPoint (x, y) = Point x y

belongsTo :: Point -> Polyomino -> Bool
point `belongsTo` (Polyomino points) = point `member` points

add :: Point -> Polyomino -> Polyomino
add point (Polyomino points) = Polyomino $ insert point points

renderPoint :: Point -> Polyomino -> String
renderPoint point polyomino = if point `belongsTo` polyomino then "[]" else "  "  

renderLine :: [Int] -> Int -> Polyomino -> String
renderLine xRange y polyomino = 
    concat [renderPoint (Point x y) polyomino | x <- xRange]

renderRectangle :: [Int] -> [Int] -> Polyomino -> String
renderRectangle xRange yRange polyomino = 
    intercalate "\n" [renderLine xRange y polyomino | y <- yRange] 

renderPolyomino :: Polyomino -> String 
renderPolyomino p = "\n" ++ (renderRectangle (xRange d) (yRange d) polyomino) ++ "\n"
    where
       polyomino = normalize p
       d = getDimensions polyomino

allRotations :: Polyomino -> [Polyomino]
allRotations p = rotations' 3 [normalize p]
    where
        rotations' 0 xs = xs
        rotations' n (x:xs) = rotations' (n - 1) ((rotate x):x:xs)
        rotate = normalize . (rotateRight $ Point 0 0)

instance Show Polyomino where
    show = renderPolyomino

instance Eq Polyomino where
    p1 == p2 = any equalToP1 $ allRotations p2
        where equalToP1 p = points p == (points $ normalize p1)

tetramino :: Polyomino
tetramino = Polyomino.fromList [(1, 0), (2, 0), (3, 0), (3, 1)]


