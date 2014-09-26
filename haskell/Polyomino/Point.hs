module Point where

data Point = Point { x :: Int, y :: Int } deriving (Show, Read, Eq)

move :: Int -> Int -> Point -> Point
move dx dy (Point x y) = Point (x + dx) (y + dy)

rotateRight :: Point -> Point -> Point
rotateRight (Point x1 y1) (Point x2 y2) = Point newX newY
    where 
        newX = -(y2 - y1) + x1
        newY =  (x2 - x1) + y1

rotateLeft :: Point -> Point -> Point
rotateLeft (Point x1 y1) (Point x2 y2) = Point newX newY
    where 
        newX = (y2 - y1) + x1
        newY = -(x2 - x1) + y1


