{-
An integer whose base-ten representation consists only of zero and one is called a  "zero-one".
 
Given an arbitrary integer N, find the string S that represents smallest positive "zero-one"  integer which is a multiple of N.  (It is mathematically guaranteed that every N has at least one S).
 
Input Format
One integer, N.
 
Output Format
Type cast the integer S to a string and return that string.
 
Constraint:
0 < N < 100,000
S should be a zero-one integer as defined above. 
There should not be any leading zeros.
 
Sample Input:
4
Sample Output:
100

-}

import Data.List

--http://stackoverflow.com/questions/28268786/how-to-solve-zero-one-multiple-coding-solution

updateBins :: [Int] -> [Int]
updateBins b = nub $ b ++ [ 10*old + new | old <- b, new <- [0,1]]

isBin n = and $ map ((flip elem) "01") $ show n

works n order bins p = elem (n*p `mod` (10^order)) bins

bar l = [i+ 10*j | i <- l, j <- [0..9]]

foo n order bins l = let
  l' = filter (works n order bins) l
  chk = filter isBin $ map (*n) l
  in if (length chk == 2) then chk else foo n (order +1) (updateBins bins) (bar l')

get :: Int -> Int
get n = let
  l = foo n 1 [0,1] [0..9]
  in l !! 1

zero_One num = show $ get num

