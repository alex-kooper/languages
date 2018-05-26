
mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = mergeSorted (mergeSort left) (mergeSort right)
  where
    (left, right) =
      let halfOfSize = length xs `div` 2
      in (take halfOfSize xs, drop halfOfSize xs)


mergeSorted :: Ord a => [a] -> [a] -> [a]
mergeSorted x [] = x
mergeSorted [] x = x
mergeSorted (x:xs) (y:ys)
  | x < y     = x : mergeSorted xs (y:ys)
  | otherwise = y : mergeSorted (x:xs) ys

main :: IO ()
main = print $ mergeSort $ reverse [1..50::Int]
