{-
Given a string, s, we define a substring, s', of s to be a string that occurs in s (i.e., a string matching some contiguous block of characters in s).
 
Complete the counting function in the editor below. It has one parameter: a string, s, consisting of only 0's and 1's. The function must return an integer denoting the total number of substrings of s satisfying both of the following two conditions:
The 0's and 1's are grouped consecutively (e.g., 01, 10, 0011, 1100, 000111, etc.).
The number of 0's in the substring is equal to the number of 1's in the substring.
 
Input Format
Locked stub code in the editor reads string s from stdin and passes it to the function.
 
Constraints
5 ≤ the length of string s ≤ 5 × 105
String s consists of 0's and 1's only.
 
Output Format
The function must return an integer denoting the number of substrings of s having an equal number of 0's and 1's that are consecutively grouped (i.e., all the 0's are on one end of the string and all the 1's are on the other end of the string). This is printed to stdout by locked stub code in the editor.
 
Sample Input 0
00110
 
Sample Output 0
3
 
Explanation 0
There are three substrings having equal numbers of consecutively grouped 0's and 1's:
0011, which is a substring of the characters from s0 to s3 (inclusive).
01, which is a substring of the characters from s1 to s2 (inclusive).
10, which is a substring of the characters from s3 to s4 (inclusive).
Thus, we return 3 as our answer.
 
Sample Input 1
10101
 
Sample Output 1
4
 
Explanation 1
There are four substrings having equal numbers of consecutively grouped 0's and 1's:
10, which is a substring of the characters from s0 to s1 (inclusive).
01, which is a substring of the characters from s1 to s2 (inclusive).
10, which is a substring of the characters from s2 to s3 (inclusive).
01, which is a substring of the characters from s3 to s4 (inclusive).
Thus, we return 4 as our answer.
 
Sample Input 2
10001
 
Sample Output 2
2
 
Explanation 2
There are two substrings having equal numbers of consecutively grouped 0's and 1's:
10, which is a substring of the characters from s0 to s1 (inclusive).
01, which is a substring of the characters from s3 to s4 (inclusive).
Thus, we return 2 as our answer.

equalParity l = let
  e = length $ filter (=='0') l
  in e*2 == length l
-}

import Data.List

subseqs ls = [t | i <- inits ls, t <- tails i, not $ null t]

evenLength l = (length l) `mod` 2 == 0

equalParity l = let
  len = (length l) `div` 2
  first = take len l
  second = drop len l
  zeroOne = (and $ map (=='0') first) && (and $ map (=='1') second)
  oneZero = (and $ map (=='1') first) && (and $ map (=='0') second)
  in zeroOne || oneZero
  
counting s = length $ filter equalParity $ filter evenLength $ subseqs s
