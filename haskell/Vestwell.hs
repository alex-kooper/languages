import Data.Char
import Data.List (sort)
import Data.Foldable
import qualified Data.Map.Strict as M
import Data.Function
import Control.Arrow

anagramKey :: String -> String
anagramKey = sort . map toLower

largestAnagramGroup :: [String] -> [String]
largestAnagramGroup = 
  map toPair
    >>> M.fromListWith (++)
    >>> M.elems
    >>> maximumBy (compare `on` length)
  where
    toPair word = (anagramKey word, [word])

main :: IO ()
main = readFile "vestwell.txt" 
  >>= (return . largestAnagramGroup . words)
  >>= print
