{-
Given an array, words, of n word strings (words[0], words[1], …, words[n−1]), choose a word from it and, in each step, remove a single letter from the chosen word if and only if doing so yields another word that is already in the library. Each successive character removal should be performed on the result of the previous removal, and you cannot remove a character if the resulting string is not an element in words (see the Explanation below for detail). The length of a string chain is the maximum number of strings in a chain of successive character removals.
 
Complete the longestChain function in your editor. It has 1 parameter: an array of n strings, words, where the value of each element wordsi (where 0 ≤ i < n) is a word. It must return a single integer denoting the length of the longest possible string chain in words.
 
Input Format
The locked stub code in your editor reads the following input from stdin and passes it to your function:
The first line contains an integer, n, the size of the words array. Each line i of the n subsequent lines (where 0 ≤ i < n) contains an integer describing the respective strings in words.
 
Constraints
1 ≤ n ≤ 50000
1 ≤ |wordsi| ≤ 60, where 0 ≤ i < n
Each string in words is composed of lowercase ASCII letters.
 
Output Format
Your function must return a single integer denoting the length of the longest chain of character removals possible. This is printed to stdout by the locked stub code in your editor.
 
Sample Input 1
6
a
b
ba
bca
bda
bdca
 
Sample Output 1
4
 
Explanation
Sample Case 1: words = {"a", "b", "ba", "bca", "bda", "bdca"}
Because "a" and "b" are single-character words, we cannot remove any characters from them as that would result in the empty string (which is not an element in words), so the length for both of these string chains is 1.
 
The word "ba" can create two different string chains of length 2 ("ba" → "a" and "ba" → "b"). This means our current longest string chain is 2.
 
The word "bca" can create two different string chains of length 3 ("bca" → "ba" → "a" and "bca" → "ba" → "b"). This means our current longest string chain is now 3.
 
The word "bda" can create two different string chains of length 3 ("bda" → "ba" → "a" and "bda" → "ba" → "b"). At this point, our current longest string chain is still 3.
 
The word "bdca" can create four different string chains of length 4 ("bdca" → "bda" → "ba" → "a", "bdca" → "bda" → "ba" → "b", "bdca" → "bca" → "ba" → "a", and "bdca" → "bca" → "ba" → "b"). This means our current longest string chain is now 4.
 
Having determined that 4 is the maximum string chain length for any string in words, we return 4.
-}

import Data.List 
import qualified Data.Set as S

inputs = ["a", "b", "ba", "bca", "bda", "bdca"]

chainLength :: S.Set String -> String -> Int
chainLength dict word = let
  subs = map (\x -> word \\ (x:[])) word
  out = 1 + (maximum $ map (chainLength dict) subs)
  in if S.notMember word dict then 0 else out

longestChain words = let
  dict = S.fromList words
  lengths = map (chainLength dict) words
  in maximum lengths
