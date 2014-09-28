module Polyomino (
    Polyomino(..),
    Dimensions(..),
    upperLeftCorner,
    getDimensions,
    move,
    rotateRight,
    rotateLeft,
    rotate,
    normalize,
    fromList,
    belongsTo,
    renderPolyomino    
) where

import Data.List

import Point(Point(..))
import qualified Point

data Polyomino = Polyomino { points :: [Point] } 

data Dimensions = Dimensions { 
    xRange :: [Int], 
    yRange :: [Int] 
} deriving (Show, Eq)

upperLeftCorner :: Polyomino -> Point
upperLeftCorner (Polyomino points) = Point x y
    where
        x = minimum $ map Point.x points
        y = minimum $ map Point.y points 
 
getDimensions :: Polyomino -> Dimensions
getDimensions (Polyomino points) = Dimensions [x1..x2] [y1..y2] 
    where
        x1 = minimum $ map Point.x points
        x2 = maximum $ map Point.x points
        y1 = minimum $ map Point.y points 
        y2 = maximum $ map Point.y points

move :: Int -> Int -> Polyomino -> Polyomino
move dx dy (Polyomino points) = Polyomino $ map (Point.move dx dy) points

rotateLeft :: Point -> Polyomino -> Polyomino
rotateLeft point (Polyomino points) = Polyomino $ map (Point.rotateLeft point) points

rotateRight :: Point -> Polyomino -> Polyomino
rotateRight point (Polyomino points) = Polyomino $ map (Point.rotateRight point) points

rotate p = normalize $ rotateRight (Point 0 0) p

normalize :: Polyomino -> Polyomino
normalize p = move (-x) (-y) p
    where Point x y = upperLeftCorner p
 
fromList :: [(Int, Int)] -> Polyomino
fromList xs = Polyomino $ map toPoint xs 
    where toPoint (x, y) = Point x y

belongsTo :: Point -> Polyomino -> Bool
point `belongsTo` (Polyomino points) = point `elem` points

renderPoint :: Point -> Polyomino -> String
renderPoint point polyomino = if point `belongsTo` polyomino then "[]" else "  "  

renderLine :: [Int] -> Int -> Polyomino -> String
renderLine xRange y polyomino = 
    intercalate "" [renderPoint (Point x y) polyomino | x <- xRange]

renderRectangle xRange yRange polyomino = 
    intercalate "\n" [renderLine xRange y polyomino | y <- yRange] 

renderPolyomino :: Polyomino -> String 
renderPolyomino p = "\n" ++ (renderRectangle (xRange d) (yRange d) polyomino) ++ "\n"
    where
       polyomino = normalize p
       d = getDimensions polyomino

instance Show Polyomino where
    show = renderPolyomino

tetramino :: Polyomino
tetramino = fromList [(1, 0), (2, 0), (3, 0), (3, 1)]


