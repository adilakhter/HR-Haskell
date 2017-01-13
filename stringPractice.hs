{-
We define a palindrome to be a sequence of characters that reads the same backward as it does forward. For example, tacocat and 12221 are palindromes, but tacocats and 8675 are not.
 
We define a subsequence to be a sequence formed by copying a contiguous section of elements from a sequence and deleting 0 or more characters from it.
 
Walter wants to improve his understanding of strings, so he asks Monica to give him a string problem. She creates a string, s, of n lowercase English letters and tells him to find the length of the longest subsequence of s such that the subsequence is also a palindrome.
 
Complete the longestPalindrome function in the editor below. It has two parameters:
An integer, n, denoting the number of characters in Monica's string.
A string, s, denoting Monica's string.
The function must return an integer denoting the length of the longest palindromic subsequence of s.
 
Input Format
Locked stub code in the editor reads the following input from stdin and passes it to the function:
The first line contains an integer, n, denoting the number of characters in Monica's string.
The second line contains a string, s, denoting Monica's string.
 
Constraints
1 ≤ n ≤ 5000
String s consists of lowercase English alphabetic letters.
 
Output Format
The function must return an integer denoting the length of the longest palindromic subsequence of s. This is printed to stdout by locked stub code in the editor.
 
Sample Input 0
2
ba
 
Sample Output 0
1
 
Explanation 0
The only palindromic subsequences of s = "ab" are "a" and "b". As both of these have length 1, we return 1 as our answer.
 
Sample Input 1
3
aaa
 
Sample Output 1
3
 
Explanation 1
The string s = "aaa" is already palindromic, so we return 3 as our answer.
 
Sample Input 2
6
banana
 
Sample Output 2
5
 
Explanation 2
The longest palindromic subsequence of s = "banana" is "anana", so we return 5 as our answer.
-}

--see also https://hackage.haskell.org/package/palindromes-0.1/docs/src/Data-Algorithm-Palindromes-Palindromes.html#longestPalindrome

import Data.Char

clean = map toLower . filter isAlpha

palindrome str = str == reverse str

substrings []     = []
substrings (x:xs) = substrings' (x:xs) ++ substrings xs where
  substrings' []     = []
  substrings' (y:ys) = [y] : [ (y:s) | s <- substrings' ys ]

longest []     = []
longest (x:xs) = if length x > length max then x else max
  where max = longest xs

longestPalindrome n s = length $ longest (filter palindrome (substrings (clean s)))
