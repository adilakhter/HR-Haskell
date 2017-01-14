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

  
primeFactors n =
  case factors of
    [] -> [n]
    _  -> factors ++ primeFactors (n `div` (head factors))
  where factors = take 1 $ filter (\x -> (n `mod` x) == 0) [2 .. n-1]

bins :: Int -> [Int]
bins 1 = [0,1]
bins n = let
  b = bins (n-1)
  in nub $ b ++ [ 10*old + new | old <- b, new <- [0,1]]

isBin n = and $ map ((flip elem) "01") $ show n

works n order p = elem (n*p `mod` (10^order)) $ bins n

foo n order l = let
  l' = filter (works n order) l
  chk = filter isBin $ map (*n) l
  in if (length chk == 2) then chk else foo n (order +1) (bar l')

bar l = [i+ 10*j | i <- l, j <- [0..9]]

get :: Int -> Int
get n = let
  l = foo n 1 [0..9]
  in l !! 1

zero_One num = show $ get num

