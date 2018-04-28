-- This is a solution to the problem described here:
-- http://codingdojo.org/kata/Bowling/

-- It does not include validation yet but can work for partial games
-- The tests should be converted to unit tests.

import Data.Semigroup

type Score = Int

data Frame
  = Strike
  | Spare Score
  | Open Score Score

  -- can be used as a bonus, when the last roll
  -- is a strike or a spare
  | Incomplete Score

  -- used to avoid edge cases when there is no next frames
  | Empty
  deriving (Eq)

rollsFromFrame :: Frame -> [Score]
rollsFromFrame Strike = [10]
rollsFromFrame (Spare s) = [s, 10 - s]
rollsFromFrame (Open s1 s2) = [s1, s2]
rollsFromFrame (Incomplete s) = [s]
rollsFromFrame Empty = [0] -- empty is treated like incomplete with 0 score

frameScore :: Frame -> Score -> Score -> Score
frameScore Strike next1 next2 = 10 + next1 + next2
frameScore (Spare _) next _ = 10 + next
frameScore (Open s1 s2) _ _ = s1 + s2
frameScore (Incomplete s) _ _ = s
frameScore Empty _ _ = 0

instance Show Frame where
  show Strike = "X"
  show (Spare s) = show s <> "/"
  show (Open s1 s2) = show s1 <> show s2
  show (Incomplete s) = show s
  show Empty = "0"

charToScore :: Char -> Score
charToScore c = read [c] :: Score

instance Read Frame where
  readsPrec _ ['X']  = [(Strike, [])]
  readsPrec _ [c] = [(Incomplete $ charToScore c, [])]

  readsPrec _ [c1, c2] =
    let frame = case c2 of
          '/' -> Spare $ charToScore c1
          '-' -> Open (charToScore c1) 0
          _   -> Open (charToScore c1) (charToScore c2)
    in [(frame, [])]

  readsPrec _ _ = []


parseGame :: String -> [Frame]
parseGame = map read . words

framesScore :: [Frame] -> Int
framesScore xs =
  sum $ zipWith3
    frameScore'
    (take 10 xs)
    (drop 1 xs <> [Empty])
    (drop 2 xs <> replicate 2 Empty)
  where
    frameScore' frame nextFrame1 nextFrame2 =
      let [nextRoll1, nextRoll2] = rollsFrom2Frames nextFrame1 nextFrame2
      in frameScore frame nextRoll1 nextRoll2

    rollsFrom2Frames frame1 frame2 =
      take 2 $ rollsFromFrame frame1 <> rollsFromFrame frame2

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
