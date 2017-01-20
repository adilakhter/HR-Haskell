{-
We define the following:
A binary string is a string consisting only of 0's and/or 1's. For example, 01011, 1111, and 00 are all binary strings.
The prefix of a string is any substring of the string that includes the beginning of the string. For example, the prefixes of 11010 are 1, 11, 110, 1101, and 11010.
 
We consider a non-empty binary string to be magical if the following two conditions are true:
The number of 0's is equal to the number of 1's.
For every prefix of the binary string, the number of 1's should not be less than the number of 0's.
For example, 11010 is not magical because it doesn't have an equal number of 0's and 1's, but 110100 is magical because it satisfies both of the above conditions.
 
A magical string can contain multiple magical substrings. If two consecutive substrings are magical, then we can swap the substrings as long as the resulting string is still a magical string. Given a magical binary string, str, perform zero or more swap operations on its consecutive magical substrings such that the resulting string is as lexicographically large as possible. Two substrings are considered to be consecutive if the last character of the first substring occurs exactly one index before the first character of the second substring.
 
Complete the largestMagical function in the editor below. It has 1 parameter: a string, str. It must return a string denoting the lexicographically largest possible magical string that can be formed by performing zero or more swap operations on consecutive magical substrings of str.
 
Input Format
Locked stub code in the editor reads a single binary string, str, from stdin and passes it to the function.
 
Constraints
It is guaranteed that str is a binary string of 1's and 0's only.
1 ≤ length(str) ≤ 50
It is guaranteed that str is a magical string.
 
Output Format
The function must return a string denoting the lexicographically largest magical string that can be formed from str. This is printed to stdout by locked stub code in the editor.
 
Sample Input 0
11011000
 
Sample Output 0
11100100
 
Explanation 0
Given the magical string str = 11011000, we can choose two consecutive magical substrings, 1100 and 10, to swap such that the resultant string, str' = 11100100, is the lexicographically largest possible magical string possible. Thus, we return the value of str', which is 11100100, as our answer.
 
Sample Input 1
1100
 
Sample Output 1
 
1100
 
Explanation 1
The only magical substring of str is 1100. So none of the operations can be applied on the string.
 
Sample Input 2
1101001100
 
Sample Output 2
1101001100
 
Explanation 2
The only consecutive magical substrings of str are 110100 and 1100 (note that 100 is not a magical substring because it contains more zeroes than ones); if we were to swap them, it would result in a lexicographically smaller string. Thus, str is already the lexicographically largest magical string that can be formed and we return 1101001100 as our answer.
-}

--note that the concatenation of any two magical strings is magical. also any prefix of a magical string meeting cond 1 is magical

import Data.List

subseqs ls = [t | i <- inits ls, t <- tails i, not $ null t]

isMagical str = let
  s = scanl agg (0,0) str
  agg (zeros,ones) next = case next of
    '0' -> (zeros+1,ones)
    '1' -> (zeros,ones+1)
  in (even $ length str) && (and $ map (\(z,o) -> z <= o) s)

byLen s t | (length s) > (length t) = GT
          | (length s) > (length t) = LT
          | otherwise = EQ

-- only handes one swap of suffix kind
test :: String -> String          
test s = let
  magics = filter isMagical $ subseqs s
  biggest = last $ sort magics
  longest = last $ sortBy byLen magics
  in if longest == biggest then s else swaps s longest biggest

swaps :: String -> String -> String -> String
swaps s longest biggest = let
  l = length longest
  b = length biggest
  long = take (l-b) longest
  big = biggest ++ long
  s' = s \\ longest
  first = map fst $ takeWhile (\(a,b) -> a==b) $ zip s' s
  in first ++ big ++ (s' \\ first)
  
chop :: ([a] -> (b, [a])) -> [a] -> [b]
chop _ [] = []
chop f as = b : chop f as'
  where (b, as') = f as

group :: (Eq a) => [a] -> [[a]]
group = chop (\ xs@(x:_) -> span (==x) xs)

out = test "110011110000111111000000" 

largestMagical str = id
