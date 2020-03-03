module BuildTreeFromTraversals where
import Control.Monad.State
import Data.List

data Tree a =
  Empty |
  Node a (Tree a) (Tree a)
  deriving (Eq, Show)

buildTreeM :: Eq a => [a] -> State [a] (Tree a)
buildTreeM [] = return Empty
buildTreeM inOrder = do
  root <- gets head
  modify tail
  
  let (left, rightWithRoot) = span (/= root) inOrder
  let right = tail rightWithRoot
  
  leftTree <- buildTreeM left
  rightTree <- buildTreeM right

  return $ Node root leftTree rightTree

buildTree :: Eq a => [a] -> [a] -> Tree a
buildTree inOrder preOrder = evalState (buildTreeM inOrder) preOrder

preOrder = ['A', 'B', 'D', 'E', 'C', 'F']
inOrder = ['D', 'B', 'E', 'A', 'F', 'C']

main :: IO ()
main = print $ buildTree inOrder preOrder
