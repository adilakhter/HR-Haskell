{-
A binary tree is a data structure characterized by the following properties:
It can be an empty tree, where root = null.
It can consist of a root node that contains a value, and two sub-trees, labelled , "left" and "right". Each sub-tree also follows the definition of a binary tree.
 
A binary tree is a binary search tree (BST) if all the non-empty nodes follow these two properties:
 
If node has a left sub-tree, then all the values in its left sub-tree are smaller than its value.
If node has a right sub-tree, then all the values in its right sub-tree are greater than its value.
 
 
 
We are given a N nodes, each having unique value in [1, N]. How many different binary search trees (BST's) are possible using all of them.
 
 
 
Input
The first line will contain an integer T, denoting the number of test cases. T lines follow, each representing a test case. Each test case consists a single integer, N, denoting the number of nodes in the binary search tree.
 
Output
For each test case, find the number of different binary search tree that can be created using these nodes. Print the answer modulo (10^8+7).
 
Constraints
1 <= T <= 1000
1 <= N <= 1000
 
Sample Input
 
5
1
2
3
4
100
 
Sample Output
 
1
2
5
14
25666077
Explanation
Test Case #1: We have only one tree.
 
1
Test Case #2: Two tree can be created using two noes.
 
1               2
  \            /
    2       1
 
Test Case #3:
 
1          1         2         3        3
 \          \       / \       /        /
  2          3     1   3     1        2
   \        /                 \      /
    3      2                   2    1
-}

-- can be simplified to counting # of (one level deeper) leaf-labelled trees w a given fringe
data Tree = Leaf Int | Fork Tree Tree deriving Show


trees :: [Int] -> [Tree]
trees [x] = [Leaf x]
trees (x:xs) = (trees xs) >>= (prefixes x)

prefixes :: Int -> Tree -> [Tree]
prefixes x t@(Leaf y) = [Fork (Leaf x) t]
prefixes x t@(Fork u v) = [Fork (Leaf x) t] ++ [Fork u' v | u' <- prefixes x u]

test n = let
  l = [1..n+1]
  in length $ trees l

output = map test [1,2,3,4]
  
  
{-
import Data.Tree
import Data.List

type BST = Tree Int
trees :: [Int] -> [BST]
trees [x] = [Node x []]
trees (x:xs) = concatMap (prefixes x) (trees xs)

prefixes :: Int -> BST -> [BST]
prefixes x t@(Node y []) = [Node y [Node x [],t]]
prefixes x t@(Node y [u,v]) = [Node y [Node x [],t]] ++ [Node y [u',v] | u' <- prefixes x u]
-}


{- 
import qualified Data.List

{- DEF data structure -}
data Tree a = Nil | Node (Tree a) a (Tree a) deriving Show


  
{- BASIC Information -}
empty :: (Ord a) => Tree a -> Bool
empty Nil = True
empty  _  = False

contains :: (Ord a) => (Tree a) -> a -> Bool
contains Nil _ = False
contains (Node t1 v t2) x 
  | x == v = True
  | x  < v = contains t1 x 
  | x  > v = contains t2 x

{- BASIC Manipulation -}
insert :: (Ord a) => Tree a -> a -> Tree a
insert Nil x = Node Nil x Nil
insert (Node t1 v t2) x 
  | v == x = Node t1 v t2
  | v  < x = Node t1 v (insert t2 x)
  | v  > x = Node (insert t1 x) v t2

delete :: (Ord a) => Tree a -> a -> Tree a
delete Nil _ = Nil
delete (Node t1 v t2) x  
  | x == v = deleteX (Node t1 v t2)
  | x  < v = Node (delete t1 x) v t2
  | x  > v = Node t1 v (delete t2 x)

-- Delete root (is used on subtree)
deleteX :: (Ord a) => Tree a -> Tree a 
deleteX (Node Nil v t2) = t2
deleteX (Node t1 v Nil) = t1
deleteX (Node t1 v t2) = (Node t1 v2 t2) --(delete t2 v2))
  where 
    v2 = leftistElement t2

-- Return leftist element of tree (is used on subtree)
leftistElement :: (Ord a) => Tree a -> a
leftistElement (Node Nil v _) = v
leftistElement (Node t1 _ _) = leftistElement t1

-- Create tree from list of elemtents
ctree :: (Ord a) => [a] -> Tree a
ctree [] = Nil
ctree (h:t) = ctree2 (Node Nil h Nil) t
  where
    ctree2 tr [] = tr
    ctree2 tr (h:t) = ctree2 (insert tr h) t

-- Create perfect balance BST
ctreePB :: (Ord a) => [a] -> Tree a
ctreePB [] = Nil
ctreePB s = cpb Nil (qsort s) 

cpb :: (Ord a) => Tree a -> [a] -> Tree a
cpb tr [] = tr
cpb tr t = cpb (insert tr e) t2
  where  
    e = middleEl t
    t2 = Data.List.delete e t

-- Element in middle
middleEl :: (Ord a) => [a] -> a
middleEl s = mEl s s 

mEl :: (Ord a) => [a] ->  [a] -> a
mEl    []    (h:s2) = h
mEl (_:[])   (h:s2) = h
mEl (_:_:s1) (_:s2) = mEl s1 s2

{- PRINT -}
inorder :: (Ord a) => Tree a -> [a]
inorder Nil = []
inorder (Node t1 v t2) = inorder t1 ++ [v] ++ inorder t2

preorder :: (Ord a) => Tree a -> [a]
preorder Nil = []
preorder (Node t1 v t2) = [v] ++ preorder t1 ++ preorder t2

postorder :: (Ord a) => Tree a -> [a]
postorder Nil = []
postorder (Node t1 v t2) = postorder t1 ++ postorder t2 ++ [v]

-- from wiki
levelorder :: (Ord a) => Tree a -> [a]
levelorder t = step [t]
  where
    step [] = []
    step ts = concatMap elements ts ++ step (concatMap subtrees ts)
    elements Nil = []
    elements (Node left x right) = [x]
    subtrees Nil = []
    subtrees (Node left x right) = [left,right]

qsort :: (Ord a) => [a] -> [a] 
qsort [] = []
qsort (h:t) = (qsort [x| x<-t, x < h]) ++ [h] ++ (qsort [x| x<-t, x>=h ])

-}
