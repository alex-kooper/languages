import qualified Data.Map as Map
import Data.List
import Data.Char
import Data.Function
import Data.Ord
import System.Environment

stringToWords :: String -> [String]
stringToWords = (filter $ any isAlpha) . groupBy ((==) `on` isAlpha)

countWords :: String -> [(String, Int)]
countWords s =
    let words = stringToWords s
        m  = Map.fromListWith (+) $ map (\w -> (w, 1)) words
    in  take 10 $ reverse $ sortOn snd $ Map.toList m

main = do
    args <- getArgs
    case args of
        [fileName] -> do
            m <- countWords <$> readFile fileName
            mapM_ putStrLn $ map formatPair m
        _ -> putStrLn "Invalid arguments!"

    where
        formatPair (k, v) = k ++ " : " ++ show v
