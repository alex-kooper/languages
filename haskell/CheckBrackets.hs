import qualified Data.Map as Map
import Data.Maybe
import Control.Monad
import System.Environment
import Text.Printf

data Token = Token {
    char :: Char,
    line :: Int,
    pos :: Int
} deriving (Show)

type Error = String

bracketMap :: Map.Map Char Char
bracketMap = Map.fromList [('(', ')'), ('[', ']'), ('{', '}')]

isLeftBracket :: Token -> Bool
isLeftBracket (Token c _ _) = isJust $ Map.lookup c bracketMap

isRightBracket :: Token -> Bool
isRightBracket (Token c _ _) = c `elem` Map.elems bracketMap

isBracket :: Token -> Bool
isBracket b = isLeftBracket b || isRightBracket b

bracketsMatch :: Token -> Token -> Bool
bracketsMatch l r = Map.lookup (char l) bracketMap == Just (char r)

errorLeftIsMissing :: Token -> String
errorLeftIsMissing (Token c l p) =
    printf "Line: %d pos: %d - left bracket is missing for: '%c'" l p c

errorRightIsMissing :: Token -> String
errorRightIsMissing (Token c l p) =
    printf "Line: %d pos: %d - right bracket is missing for: '%c'" l p c

errorDoNotMatch :: Token -> Token -> String
errorDoNotMatch (Token c1 l1 p1) (Token c2 l2 p2)  =
    printf ("The brackets do not match\n" ++
            "Line: %d pos: %d - '%c'\n" ++
            "Line: %d pos: %d - '%c'\n")
            l1 p1 c1 l2 p2 c2

tokens :: String -> [Token]
tokens s = [Token c ln p | (ln, l) <- zip [1..] $ lines s,
                           (p, c) <- zip [1..] l]


acceptChar :: [Token] -> Token -> Either Error [Token]

acceptChar xs c | not $ isBracket c = Right xs
                | isLeftBracket c   = Right $ c : xs

acceptChar [] c = Left $ errorLeftIsMissing c

acceptChar (x:xs) c | bracketsMatch x c = Right xs
                    | otherwise = Left $ errorDoNotMatch x c


checkBrackets :: String -> Maybe Error
checkBrackets s =
    let stack = foldM acceptChar [] $ tokens s
    in  case stack of
            Right [] -> Nothing
            Left err -> Just err
            Right xs -> Just $ errorRightIsMissing $ head xs

main :: IO ()
main = do
    [fileName] <- getArgs
    contents <- readFile fileName
    putStrLn $ fromMaybe "All brackets match!" $ checkBrackets contents
