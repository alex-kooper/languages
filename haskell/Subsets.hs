module Subsets where

import Control.Monad
import Data.Monoid

subsetsRecursive :: Eq a => [a] -> [[a]]
subsetsRecursive = filter (/= []) . subsets
  where
    subsets [] = [[]]
    subsets (x:xs) =
      let tailSubsets = subsets xs
      in  fmap (x:) tailSubsets <> tailSubsets

subsetsMonadic :: Eq a => [a] -> [[a]]
subsetsMonadic = filter (/= []) . filterM (const [True, False])

combinations :: [a] -> Int -> [[a]]
combinations _ 0 = [[]]
combinations [] _ = []
combinations (x:xs) n
  | length xs >= (n - 1) = ((x:) <$> combinations xs (n - 1)) <> combinations xs n
  | otherwise            = []
