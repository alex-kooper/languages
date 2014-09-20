import Data.Char
import Data.Maybe
import Data.List.Split

import qualified Data.Map as Map 
import qualified Data.Set as Set 

type NodeName = String
type TreeMap = Map.Map NodeName (Maybe NodeName, Maybe NodeName)

leftChild :: NodeName -> TreeMap -> Maybe NodeName
leftChild name tree = maybe Nothing fst (Map.lookup name tree)

rightChild :: NodeName -> TreeMap -> Maybe NodeName
rightChild name tree = maybe Nothing snd (Map.lookup name tree)

isLeaf name tree = 
    (isNothing $ leftChild name tree) && (isNothing $ rightChild name tree)

fromTriplets :: [(NodeName, Maybe(NodeName), Maybe(NodeName))] -> TreeMap
fromTriplets triplets = Map.fromList $ map tripletToPair triplets
    where tripletToPair (p, l, r) = (p, (l, r)) 

fromString :: String -> TreeMap
fromString = fromTriplets . (map readLine) . lines
    where readLine = toTriplet . (splitOn ",") . (filter $ not . isSpace)
          toTriplet [a, b, c] = (a, toMaybe b, toMaybe c)
          toTriplet _         = error("Bad triplet string format!!!")
          toMaybe x = if null x then Nothing else Just x

findRoots :: TreeMap -> [NodeName]
findRoots tree = Set.toList $ Set.difference (parents tree) (children tree)
    where  
        parents = Set.fromList . Map.keys
        children = Map.foldr processOne Set.empty

        processOne (Just left, Just right) = (Set.insert left) . (Set.insert right)
        processOne (Just left, Nothing) = Set.insert left
        processOne (Nothing, Just right) = Set.insert right
        processOne (Nothing, Nothing) = id 

treeNodeMinDepth :: NodeName -> TreeMap -> Integer
treeNodeMinDepth root tree 
    | isLeaf root tree = 0
    | isNothing left   = depthOfRight + 1 
    | isNothing right  = depthOfLeft + 1
    | otherwise        = (min depthOfLeft depthOfRight) + 1 
    where
        left = leftChild root tree
        right = rightChild root tree

        depthOfLeft = treeNodeMinDepth (fromJust left) tree
        depthOfRight = treeNodeMinDepth (fromJust right) tree

treeMinDepth :: TreeMap -> Integer
treeMinDepth tree = minimum [treeNodeMinDepth r tree | r <- findRoots tree]

main = do
    contents <- readFile "tree.txt"
    let tree = fromString contents

    putStrLn $ "Tree roots: " ++ (show $ findRoots tree)
    putStrLn $ "Trees minimum depth is: " ++ (show $ treeMinDepth tree)

