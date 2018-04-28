-- This is a solution to the problem described here:
-- http://codingdojo.org/kata/Bowling/

-- It does not include validation yet but can work for partial games
-- The tests should be converted to unit tests.

module Bowling where

import Data.Semigroup
import Data.Maybe

type Roll = Int

data Frame = Frame Roll (Maybe Roll) deriving (Eq)

-- Empty frames are used to avoid special cases, when there is not
-- next rolls
emptyFrame :: Frame
emptyFrame = Frame 0 Nothing

rollsFromFrame :: Frame -> [Roll]
rollsFromFrame (Frame t1 t2) = [t1] <> maybeToList t2

isStrike :: Frame -> Bool
isStrike (Frame t1 _) = t1 == 10

isSpare :: Frame -> Bool
isSpare (Frame t1 (Just t2)) = t1 + t2 == 10
isSpare (Frame _ _) = False

instance Show Frame where
  show (Frame 10 Nothing) = "X"
  show (Frame x Nothing) = show x
  show (Frame x (Just y))
    | x + y == 10 = show x <> "/"
    | otherwise   = show x <> show y

instance Read Frame where
  readsPrec _ ['X']  = [(Frame 10 Nothing, [])]
  readsPrec _ [char] = [(Frame (read [char] :: Int) Nothing, [])]

  readsPrec _ [char1, char2] =
    let t1 = read [char1] :: Int
        t2 = case char2 of
          '/' -> 10 - t1
          '-' -> 0
          c   -> read [c] :: Int
    in [(Frame t1 (Just t2), [])]

  readsPrec _ _ = error "Cannot parse a frame"


parseGame :: String -> [Frame]
parseGame = map read . words

framesScore :: [Frame] -> Int
framesScore xs =
  sum $ zipWith3
    frameScore
    (take 10 xs)
    (drop 1 xs <> [emptyFrame])
    (drop 2 xs <> replicate 2 emptyFrame)
  where
    frameScore frame nextFrame1 nextFrame2 =
      let [nextRoll1, nextRoll2] = rollsFrom2Frames nextFrame1 nextFrame2
      in frameScore' frame nextRoll1 nextRoll2

    rollsFrom2Frames frame1 frame2 =
      take 2 $ rollsFromFrame frame1 <> rollsFromFrame frame2

    frameScore' frame @ (Frame roll1 roll2) nextRoll1 nextRoll2
      | isStrike frame = 10 + nextRoll1 + nextRoll2
      | isSpare  frame = 10 + nextRoll1
      | otherwise      = roll1 + fromMaybe 0 roll2

gameScore :: String -> Int
gameScore = framesScore . parseGame

-- Testing different cases

test1 :: String
test1 = "X X X X X X X X X X X X"

test2 :: String
test2 = "9- 9- 9- 9- 9- 9- 9- 9- 9- 9-"

test3 :: String
test3 = "5/ 5/ 5/ 5/ 5/ 5/ 5/ 5/ 5/ 5/ 5"

showTest :: String -> IO()
showTest test = putStrLn
  ("Game score for: " <> test <> " = " <> show (gameScore test))

main :: IO()
main = do
  showTest test1
  showTest test2
  showTest test3
