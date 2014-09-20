import Data.List hiding (permutations)
import qualified Data.Map as Map 
import Data.Char

permutations :: Eq a => [a] -> Int -> [[a]]
permutations xs len 
    | len < 0         = error("permutations: len cannot be negative")
    | length xs < len = []
    | len == 0        = [[]]
    | otherwise       = [x:y | x <- xs, y <- permutations (delete x xs) (len - 1)]

letterMaps :: [Char] -> [Map.Map Char Char]
letterMaps letters = [Map.fromList $ zip letters digits | digits <- perms]
    where perms = permutations ['0'..'9'] (length letters)

replace :: [Char] -> Map.Map Char Char -> [Char]
replace letters letterMap = map mapLetter letters 
    where mapLetter l = Map.findWithDefault l l letterMap

candidates :: String -> [String]
candidates crypt = [replace crypt letterMap | letterMap <- letterMaps letters]
    where letters = nub $ filter isAlpha crypt

isSolution :: String -> Bool
isSolution = check . words
    where check [a, "+", b, "=", c] 
              | "0" `isPrefixOf` a  = False 
              | "0" `isPrefixOf` b  = False 
              | "0" `isPrefixOf` c  = False 
              | otherwise         = (read a) + (read b) == (read c)

solve :: String -> [String]
solve = (filter isSolution) . candidates

crypt = "SEND + MORE = MONEY"

main = do
    putStrLn $ "Solving: " ++ crypt
    let solutions = solve crypt
    putStrLn $ "Found " ++ show (length solutions) ++ " solutions"
    mapM_ putStrLn solutions
 
