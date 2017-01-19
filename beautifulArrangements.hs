{-
Julia has N numbers from 1 to N. Her arrangement of those numbers is called a beautiful arrangement if any of the following is true:
The number present at the ith position is divisible by i.
i is divisible by the number present at the ith position.
 
Complete the arrangements function in your editor. It has 1 parameter:
An integer N.
 
It must return the number of beautiful arrangements possible.
 
Input Format
The locked stub code in your editor reads the following input from stdin and passes it to your function:
The first line contains an integer, N.
 
Constraints
1 < N < 20
 
Output Format
Your function must return the total number of all possible beautiful arrangements. This is printed to stdout by the locked stub code in your editor.
 
Sample Input 1
2
Sample Output 1
2
 
Explanation 1
The number of possible beautiful arrangements for N=2 will be:
1 2
2 1
 
Consider the arrangement [1, 2] then:
number present at 1st position (i = 1) is 1, and 1 is divisible by i.
number present at 2nd position (i = 2) is 2, and 2 is divisible by i.
 
Consider the arrangement [2, 1] then:
number present at 1st position (i = 1) is 2, and 2 is divisible by i.
number present at 2nd position (i = 2) is 1, and i is divisible by 1.
 
Sample Input 2
3
Sample Output 2
3
 
Sample Input 3
4
 
Sample Output 3
8

sudokuSolve :: Board -> [Board]
sudokuSolve problem = let
  indices = filter (notPlayed problem) [0..80]
  sudokuIter indices = case indices of
    [] -> [problem]
    index : tail -> do
      board <- sudokuIter tail
      k <- [1..9]
      guard $ isLegal board (index,k)
      return $ (index,k) : board 
  in sudokuIter indices

      soln <- solve xs
      i <- [1..n]
      let index = next soln
      guard $ isLegal (index,i) soln
      let soln' = (index,i):soln
      return $ soln':beaut

arrangements n = undefined
-}

--import Data.Vector as V
import Control.Monad (guard)
import Data.List 
import Data.Maybe (fromMaybe)

import Debug.Trace

type Beaut = [(Int,Int)]

d `divides` n = n `mod` d == 0

apply soln = let
  outputs = map snd soln
  applied = map (\i -> (i,fromMaybe 1 $ lookup i soln)) outputs
  in applied

unique :: Beaut -> Bool
unique soln = let
  indices = map fst soln
  vals = map snd soln
  l = length soln
  in (l == (length $ nub indices)) && (l == (length $ nub vals))
  
legal :: Beaut -> Bool
legal soln = let
  check = map (\(p,q) -> p `divides` q)
  p = check soln
  q = check $ apply soln
  in (unique soln) && (and $ map (\(p,q) -> p||q) $ zip p q)

arrangements n = let
  solve beaut = case beaut of
    [1] -> [[(1,i)] | i <- [1..n]]
    idx:rest -> do
      soln <- solve rest
      i <- [1..n]
      let soln' = (idx,i) : soln
      guard $ legal soln'
      return soln'
  in length $ solve $ reverse [1..n]
