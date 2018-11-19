import Data.Char

numberToDigits :: Int -> [Int]
numberToDigits = map digitToInt . show

checkSum :: Int -> Int
checkSum n = sum finalDigits
  where
    multipliers = concat $ repeat [1, 2]
    doubled = zipWith (*) (reverse $ numberToDigits n) multipliers
    finalDigits = concatMap numberToDigits doubled

main :: IO ()
main = print $ checkSum 1234567
