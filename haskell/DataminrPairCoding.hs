import Data.Char
import Data.Map
import Data.List.Split

example :: String
example = "Aa1133Iab2Aa112Iac3"

tokens :: String -> [String]
tokens xs@('A':_) = token : tokens rest
  where
    (token, rest) = splitAt 4 xs

tokens xs@('I':_) = token : tokens rest
  where
    (token, rest) = splitAt 3 xs

tokens xs@(x:_)
  | isDigit x = let (token, rest) = span isDigit xs
                in  token : tokens rest
  | otherwise = []

tokens _ = []

parseIntoMap :: String -> Map String Int
parseIntoMap = fromListWith (+) .
               fmap toPair .
               chunksOf 2 .
               tokens
 where
   toPair [i, q] = (i, read q)

main :: IO ()
main = print $ parseIntoMap example
