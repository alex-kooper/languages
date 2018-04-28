module TreeTraversal where
import Debug.Trace

data Tree a =
  Empty |
  Node a (Tree a) (Tree a)
  deriving (Eq, Show)

tree1 :: Tree Int
tree1 =
  Node 1
    (Node 11
      (Node 111 Empty Empty)
      (Node 112 Empty Empty))
    (Node 12 Empty Empty)

preorderTraversal :: Show a => Tree a -> [a]
preorderTraversal tree = go tree []
  where
    go Empty        z = z
    go (Node v l r) z = visit v `seq` (v : go l (go r z))
    visit v = traceId ("\nTraversing: " ++ show v ++ "\n")

inorderTraversal :: Show a => Tree a -> [a]
inorderTraversal tree = go tree []
  where
    go Empty        z = z
    go (Node v l r) z = go l (visit v `seq` (v : go r z))
    visit v = traceId ("\nTraversing: " ++ show v ++ "\n")

postorderTraversal :: Show a => Tree a -> [a]
postorderTraversal tree = go tree []
  where
    go Empty        z = z
    go (Node v l r) z = go l $ go r (visit v `seq` (v : z))
    visit v = traceId ("\nTraversing: " ++ show v ++ "\n")

main :: IO ()
main = do
  print $ take 3 $ postorderTraversal tree1
  putStrLn "Done"
