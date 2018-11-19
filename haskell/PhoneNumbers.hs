import Data.Map
import Data.List

digitsToChars :: Map Char String
digitsToChars = fromList
  [ ('0', "0")
  , ('1', "1")
  , ('2', "abc")
  , ('3', "def")
  , ('4', "ghi")
  , ('5', "jkl")
  , ('6', "mno")
  , ('7', "pqrs")
  , ('8', "tuv")
  , ('9', "wxyz") ]

phoneNumberToWords :: String -> [String]
phoneNumberToWords = sequence . fmap (digitsToChars !)

phoneNumberToWordsFormatted :: String -> String
phoneNumberToWordsFormatted = intercalate "," . sort . phoneNumberToWords

main :: IO ()
main = putStrLn $ phoneNumberToWordsFormatted "2453270"
