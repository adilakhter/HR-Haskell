{-
Problem Statement
A palindrome is a word, phrase, number, or other sequence of characters which reads the same forward and backwards. For example: "madam" and "dad" are palindromes, but "sir" and "sad" are not.
 
The fun score for two subsequences, A and B, in string s is the product of their respective lengths. There are many ways to choose A and B, but your goal is to maximize the fun score. There can't be any overlap or crossover between the two subsequences.
 
Given string s, select exactly two non-overlapping palindromic subsequences, A and B, from s to maximize the fun score.
 
Constraints
1 < |s| ≤ 3000
s is composed of lowercase English characters.
 
Input Format
Complete a function named funPal which takes a single string (s) as a parameter.
 
Output Format
Return a single integer denoting the maximum possible fun score for s.
 
Sample Input 0
acdapmpomp
 
Sample Output 0
15
 
Sample Input 1
axbawbaseksqke
 
Sample Output 1
25
 
Explanation
Sample Case 0
You can select A=aca and B=pmpmp. The product is 3 * 5 = 15, so we return 15.
 
Sample Case 1
You can select A=ababa and B=ekske. The product is 5 * 5 = 25, so we print 25. Another possible solution is A=ababa and B=ekqke which also yields the answer 25.
Haskell
-}

import Data.Array
import Debug.Trace
import Control.Monad (guard,mplus)

buyable n = r!n
    where r = listArray (0,n) (True : map f [1..n])
          f i | trace (show i) False = undefined
          f i = i >= 6 && r!(i-6) || i >= 9 && r!(i-9) || i >= 20 && r!(i-20)
          
buy n = r!n
    where r = listArray (0,n) (Just (0,0,0) : map f [1..n])
          f i | trace (show i) False = undefined
          f i = do (x,y,z) <- attempt (i-6)
                   return (x+1,y,z)
                `mplus`
                do (x,y,z) <- attempt (i-9)
                   return (x,y+1,z)
                `mplus`
                do (x,y,z) <- attempt (i-20)
                   return (x,y,z+1)
          attempt x = guard (x>=0) >> r!x

answer :: String -> Int
answer s = table ! (1, l)
  where
    l = length s
    
    a :: Array Int Char
    a = listArray (1, l) s

    table :: Array (Int, Int) Int
    table = listArray ((1, 1), (l, l)) [f i j | i <- [1..l], j <- [1..l]]

    f i j | trace ((show (a!i)) ++ " " ++ (show (a!j))) False = undefined
          |    i    >     j    = 0
          |    i    ==    j    = 1
          | (a ! i) == (a ! j) = 2 + table ! (i+1, j-1)
          | otherwise          = maximum [table ! (i+1, j), table ! (i, j-1)]

funScore :: String -> Int
funScore s = table ! (1, l, 1, l)
  where
    l = length s
    
    a :: Array Int Char
    a = listArray (1, l) s

    table :: Array (Int, Int, Int, Int) Int
    table = listArray ((1,1,1,1), (l,l,l,l)) [f i j m n | i <- [1..l], j <- [1..l], m <- [1..l], n <- [1..l]]

    f i j m n | (i > j) || (m > n) = 0
              | ( i==j) || ( m==n) = 1
              | (a ! i) == (a ! j) = 2 + table ! (i+1, j-1, m, n)
              | (a ! m) == (a ! n) = 2 + table ! (i, j, m+1, n-1)
              | (m==i) || (m==j) || (n==i) || (n==j) = maximum [table ! (i+1, j, m, n), table ! (i, j-1, m, n),table ! (i, j, m+1, n), table ! (i, j, m, n-1)]
              | otherwise          = maximum [table ! (i+1, j, m, n), table ! (i, j-1, m, n),table ! (i, j, m+1, n), table ! (i, j, m, n-1)]
