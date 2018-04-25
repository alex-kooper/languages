import Control.Monad
import Data.Monoid

subsetsRecursive = filter (/= []) . subsets
  where
    subsets [] = [[]]
    subsets (x:xs) =
      let tailSubsets = subsets xs
      in  fmap (x:) tailSubsets <> tailSubsets

subsetsMonadic :: Eq a => [a] -> [[a]]
subsetsMonadic = filter (/= []) . filterM (const [True, False])

combinations :: [a] -> Int -> [[a]]
combinations (x:xs) n
  | n > length xs = (x:) <$> combinations xs (n - 1) <> combinations xs n
  | otherwise     = [[]]
