{-
We have a string of length N. The string contains only lower-case letters (a-z). Can you figure out the number of occurrences of the most frequent sub-string in this string?   
 
We are only interested in the sub-strings that follow the following properties:
The length of a sub-string lies between K and L.
The number of unique characters in the sub-string do not exceed M.
 
Complete the function frequent in your editor. It has 4 parameters:
A string, s.
An integer, k. 
An integer, l.
An integer, m.
 
It must return an integer denoting the number of occurrences of the most frequent sub-string in s.
 
Input Format
The locked stub code in your editor reads the following input from stdin and passes it to your function:
The first line contains a string s.
The next line contain an integer k.
The next line contain an integer l.
The next line contain an integer m.
 
Constraints
2 <= N <= 100000
2 <= K <= L <= 26, L < N
2 <= M <= 26
 
Output Format
Your function must return an integer denoting the number of occurrences of the most frequent sub-string in s. This is printed to stdout by the locked stub code in your editor.
 
Sample Input 1
abcde
2
4
26
 
Sample Output 1
1
Explanation 1
All sub-strings of in "abcde" having length 2, 3 or 4  occur exactly once. The number of characters in any sub-string is trivially less than 26. Answer is thus 1.
 
Sample Input 2
ababab
2
3
4
 
Sample Output 1
3

Explanation 2
Of size 2 sub-strings, we have 'ab' repeated 3 times and, 'ba' repeated twice.
Of size 3 we have 'aba' repeated twice and 'bab' repeated twice.
None of the longer sub-strings can occur more frequently than sub-string is 'ab', so answer is 3.
-}

import Data.List

import Text.Regex.Posix
import Text.Regex.Base
import Text.Regex

subseqs s k m =
  let init = [t | i <- inits s, t <- tails i, not $ null t]
      init' = filter (\x -> length x >= k) init
      out = filter (\x -> length (nub x) <= m) init'
  in out

counts :: String -> String -> Int
counts s c = s =~ c :: Int

-- slow. 3/13 tests passing
frequent :: String -> Int -> Int -> Int -> Int
frequent s k l m =
  let candidates = subseqs s k m
  in maximum $ map (counts s) candidates

