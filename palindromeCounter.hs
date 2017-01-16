{-
Complete the countPalindromes function in your editor. It has 1 parameter: a string, s. It must return an integer denoting the number of palindromic substrings of s.
 
Input Format
The locked stub code in your editor reads a single string, s from stdin and passes it to your function.
 
Constraints
1 ≤ |S| ≤ 5 × 103
s is composed of lowercase English letters.
 
Output Format
Your function must return an integer denoting the number of different palindromic substrings of s. This is printed to stdout by the locked stub code in your editor.
 
Sample Input 0
aaa
 
Sample Output 0
6
 
Sample Input 1
abccba
 
Sample Output 1
9
 
Sample Input 2
daata
 
Sample Output 2
7
 
Explanation
Sample Case 0:
There are 6 possible substrings of s: {"a", "a", "a", "aa", "aa", "aaa"}. All of them are palindromes, so we return 6.
 
Sample Case 1:
There are 21 possible substrings of s, the following 9 of which are palindromes: {"a", "a", "b", "b", "c", "c", "cc", "bccb", "abccba"}. Thus, we return 9.
 
Sample Case 2:
There are 15 possible substrings of s, the following 7 of which are palindromes: {"a", "a", "a", "aa", "ata", "d","t"}. Thus, we return 7.

-}

import Data.List
import Control.Applicative

subseqs ls = [t | i <- inits ls, t <- tails i, not $ null t]

palindrome s = s == (reverse s)

palindrome' :: (Eq a) => [a] -> Bool
palindrome' xs = p [] xs xs
   where p rev (x:xs) (_:_:ys) = p (x:rev) xs ys
         p rev (x:xs) [_] = rev == xs
         p rev xs [] = rev == xs

palindrome'' = (==) <*> reverse

-- 11/13 tests pass         
countPalindromes s = length $ filter palindrome $ subseqs s
