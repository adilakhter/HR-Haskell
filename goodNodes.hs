{-
We have a list of N nodes with each node pointing to exactly one of the N nodes, including itself. We call a node good, if it satisfies one of the following properties:
 
It is the tail node (marked as node 1)
It is pointing to the tail node (node 1)
It is pointing to a good node
 
You can change the pointers of some nodes in order to make them all good. You are given the description of the nodes. Find the minimum number of nodes that you have to change in order to make all the nodes good.
 
Input:
The first line contains an integer, N, which denotes the number of nodes. The next N lines contain N numbers, all between 1 and N. The first number is the number of the node pointed to by Node 1; the second number is the number of the node pointed to by Node 2; the third number is the number of the node pointed to by Node 3, and so on.
 
Output:
Print an integer which is the answer to the problem
 
Constraint
N is no larger than 1000.
 
Sample Input 1:
5
1
2
3
4
5
 
Sample output 1:
4
 
Explanation:
We have to change pointers for four nodes (node #2 to node #5) to point to node #1. Thus 4 changes are required.
 
Sample input 2:
5
5
5
5
5
5
Sample output 2:
1
 
Explanation:
We have to just change node #5 to point to node #1 (tail node) which will make node #5 good. Since all the other nodes point to a good node (node #5), every node becomes a good node.

Sample Input 3:
4
1
1
2
3
 
Sample Output 3:
0
 
Explanation:
Here node #1 is the tail node so also a good node. Node #2 is directly points to node #1 (tail node), so it is also a good node. Node #3 points to node #2 (which is a good node) so it is also a good node. Similarly node #4 points to other good node (node #3) and it is a good node also. Therefore we don't need any change in pointers as all of them are already good nodes.

-}

import Control.Applicative
import Control.Exception
import Control.Monad
import Data.Map hiding (map)
import Prelude hiding (lookup, foldl, filter)


type NodeSystem = Map Int Node

data Node = Node {  index :: Int,
                    nextIndex :: Int } deriving (Show)

instance Ord Node where
  (Node indexA _) `compare` (Node indexB _) = indexA `compare` indexB

instance Eq Node where
  (Node indexA nextA) == (Node indexB nextB) = indexA == indexB && nextA == nextB


makeNodeAssoc :: Int -> Int -> (Int, Node) 
makeNodeAssoc i t = (i, Node { index = i, nextIndex = t})

buildSystem :: [Int] -> NodeSystem
buildSystem inputList = fromList $ getZipList $ makeNodeAssoc <$> ZipList [0..] <*> ZipList inputList

{- Recursive function that given a pool of nodes (a NodeSystem) - possibly all nodes -
will walk over the possible path through nextIndex and return the deepest bad node in starting
from a given Node. Returns Nothing if the deepest Node is actually a Good Node. -}

findLastBad :: NodeSystem -> Node -> Maybe Node
findLastBad _ (Node _ 0) = Nothing
findLastBad _ node@(Node i t) | i == t = Just node
findLastBad pool node =
  case lookup (nextIndex node) pool of
    Nothing -> Just node
    Just n -> findLastBad (delete (index node) pool) n

{- This function simply recursively consume a list of node index.
Takes two accumulators, the acc that counts how many nods were changed,
and the system that is changed if needed ('Map.update'). -}

reconnectIfBad :: Int -> NodeSystem -> [Int] -> Int
reconnectIfBad _ _ [] = 0
reconnectIfBad acc system (x:xs) =
  case findLastBad system node of
    Just n -> 1 + reconnectIfBad acc (reconnect n) xs
    _ -> reconnectIfBad acc system xs
    where node = system ! x
          reconnect n = update (\ _ ->  Just n { nextIndex = 0 } ) (index n) system

{- This is the code function that simply 'Map.foldl' over a 'NodeSystem' producing a new one
with all Good Nodes with minimum moves. Then it how many nodes where reconnected. -}

solve :: NodeSystem -> Int
solve pool = reconnectIfBad 0 pool $ keys pool

solve $ buildSystem $ fmap (\x->x-1) [1,2,3,4,5]


