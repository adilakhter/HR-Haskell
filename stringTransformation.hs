{-
String s is an array of lowercase English characters (a-z) indexed from 0 to N − 1. Each index i (where 0 ≤ i < N) in s contains character si.
 
Iterate through each character in s and, for each index i, count the occurrences, k, of si in the inclusive range of indices from 0 to i-1. Then cyclically increment character si by k and save the resulting value to index i in a new string, q.
 
Given s, find string q and print it on a new line.
 
Note: Cyclically incrementing a lowercase character by an integer means that a + 1 = b, b + 2 = d, and z + 1 = a.
 
Input Format
The first line contains T (the number of test cases). Each test case is described over two lines.
For each test case, the first line contains an integer, N, and the second line contains a string of length N.
 
Constraints
1 ≤ T ≤ 20
1 ≤ N ≤ 105
 
Output Format
For each test case, print string q on a new line.
 
Sample Input 0
4
5
abcde
10
abcdeabcde
3
yzz
6
apapap
 
Sample Output 0
abcde
abcdebcdef
yza
apbqcr
 
Explanation
Test Case 0: s0 = "abcde"
Because each letter appears only once and no characters need to be incremented, q = s and we print the unmodified string on a new line.
 
Test Case 1: s1 = "abcdeabcde"
Indices 0 through 4 contain unique characters and thus are copied as-is into q.
s5 = 'a', which appeared once previously at index 0; therefore, q[5] = a + 1 = 'b'.
s6 = 'b', which appeared once previously at index 1; therefore, q[6] = b + 1 = 'c'.
s7 = 'c', which appeared once previously at index 2; therefore, q[7] = c + 1 = 'd'.
s8 = 'd', which appeared once previously at index 3; therefore, q[8] = d + 1 = 'e'.
s9 = 'e', which appeared once previously at index 4; therefore, q[9] = e + 1 = 'f'.
We print q, which is "abcdebcdef", on a new line.
 
Test Case 2: s2 = "yzz"
Indices 0 and 1 contain unique characters and thus are copied as-is into q.
s2 = 'z', which appeared once previously at index 1; therefore, q[2] = z + 1 = 'a'.
We print q, which is "yza", on a new line.
 
Test Case 3: s2 = "apapap"
Indices 0 and 1 contain unique characters and thus are copied as-is into q.
s2 = 'a', which appeared once previously at index 0; therefore, q[2] = a + 1 = 'b'.
s3 = 'p', which appeared once previously at index 1; therefore, q[3] = p + 1 = 'q'.
s4 = 'a', which appeared twice previously at indices 0 and 2; therefore, q[4] = a + 2 = 'c'.
s5 = 'p', which appeared twice previously at indices 1 and 3; therefore, q[5] = p + 2 = 'r'.
We print q, which is "apbqcr", on a new line.
-}

import Data.Char
import Control.Monad

getTuple :: IO (String, String)
getTuple = liftM2 (,) getLine getLine

countLetters :: String -> Char -> Int
countLetters str c = (length $ filter (== c) str) - 1

agg :: String -> Char
agg s =
  let c = head s
      inc = countLetters s c
  in chr $ 97 + (mod (inc - 97 + ord c) 26)
  
transform :: String -> String
transform s = let
  tails = tail $ scanl (flip (:)) [] s
  in map agg tails 

a = chr $ ord 'a'

main :: IO ()
main = do
  n <- readLn :: IO Int
  tups <- replicateM n getTuple
  let inputs = fmap snd tups
      outputs = fmap transform inputs
  mapM_ putStrLn outputs
  

