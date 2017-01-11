{-
Consider a list of size n, where each element is initially zero (i.e., list = {0, 0, …, 0}). We can perform the following operation on this list:
There are three integers, a, b, and k, where a and b denote 1-indexed indices in the list and a ≤ b.
For every integer in the list in the inclusive range between a and b, add k to its current value.
 
Given n and the a, b, and k values for m operations, perform all m operations on the list. Then print the maximum of the values in the final list on a new line.
 
Note: Naive solutions will only pass the first few test cases. You must write an efficient solution if you wish to pass all test cases.
 
Input Format
The first line contains two space-separated integers denoting the respective values of n and m.
Each of the m subsequent lines contains three space-separated integers describing the respective values of a, b, and k for an operation.
 
Constraints
3 ≤ n ≤ 107
1 ≤ m ≤ 2 × 105
1 ≤ a ≤ b ≤ n
0 ≤ k ≤ 109
 
Output Format
Your function must print a single integer denoting the maximum value in the final list after performing all m operations.
 
Sample Input 0
5 3
1 2 100
2 5 100
3 4 100
 
Sample Output 0
200
 
Explanation 0
We perform the following sequence of m = 3 operations on list = {0, 0, 0, 0, 0}:
Add k = 100 to every element in the inclusive range [1, 2], resulting in list = {100, 100, 0, 0, 0}.
Add k = 100 to every element in the inclusive range [2, 5], resulting in list = {100, 200, 100, 100, 100}.
Add k = 100 to every element in the inclusive range [3, 4], resulting in list = {100, 200, 200, 200, 100}.
We then print the maximum value in the final list, 200, as our answer.
 
Sample Input 1
4 3
2 3 603
1 1 286
4 4 882
 
Sample Output 1
882
 
Explanation 1
We perform the following sequence of m = 3 operations on list = {0, 0, 0, 0}:
Add k = 603 to every element in the inclusive range [2, 3], resulting in list = {0, 603, 603, 0}.
Add k = 286 to every element in the inclusive range [1, 1], resulting in list = {286, 603, 603, 0}.
Add k = 882 to every element in the inclusive range [4, 4], resulting in list = {286, 603, 603, 882}.
We then print the maximum value in the final list, 882, as our answer.

  let inputs = fmap (getStrings . snd) tups
      args = fmap (head . getInts . fst) tups
      app (arg,input) = map (convert arg) input
      outputs = map app $ zip args inputs
  mapM_ (putStrLn . output) $ map (foldr xor False) outputs

-}

import Control.Monad
import Text.Regex (splitRegex, mkRegex)
import Data.Array

tokenize :: String -> [String]
tokenize = splitRegex (mkRegex "[[:space:],]")

getInts :: IO [Int]
getInts = getLine >>= (\x -> return $ map (\x -> read x :: Int) $ tokenize x)

updates :: [[Int]] -> [(Int,Int)]
updates args = args >>= foo where
  foo [begin, end, update] = [(i,update) | i <- [begin .. end]]

-- brute force method 4/14 test cases passed
main :: IO ()
main = do
  [n,m] <- getInts
  args <- replicateM m getInts
  let arr = accumArray (+) 0 (1,n) $ updates args
  print $ foldl (max) (minBound :: Int) $ elems arr
