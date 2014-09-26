module Polyomino where

import Point(Point(..))
import qualified Point

data Polyomino = Polyomino { points :: [Point] } deriving (Show)

data Dimensions = Dimensions { 
    upperLeft :: Point, 
    lowerRight :: Point 
} deriving (Show, Eq)

getDimensions :: Polyomino -> Dimensions
getDimensions (Polyomino points) = Dimensions (Point x1 y1) (Point x2 y2)
    where
        x1 = minimum $ map Point.x points
        x2 = maximum $ map Point.x points
        y1 = minimum $ map Point.y points 
        y2 = maximum $ map Point.y points

move :: Int -> Int -> Polyomino -> Polyomino
move dx dy (Polyomino points) = Polyomino $ map (Point.move dx dy) points

normalize :: Polyomino -> Polyomino
normalize p = move dx dy p
    where
        dx = -(Point.x $ upperLeft d)
        dy = -(Point.y $ upperLeft d)
        d  = getDimensions p
       
fromList :: [(Int, Int)] -> Polyomino
fromList xs = Polyomino $ map toPoint xs 
    where toPoint (x, y) = Point x y

point `elem` (Polyomino points) = point `Prelude.elem` points
 
