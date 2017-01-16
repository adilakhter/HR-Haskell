{-
Consider three strings: text, prefix, and suffix. For each substring, sub, of text, we define the text_score as follows:
prefix_score = the highest n such that the first n characters of sub are equal to the last n characters of prefix and occur in the same exact order.
suffix_score = the highest n such that last n characters of sub are equal to the first n characters of suffix and occur in the same exact order.
text_score = prefix_score + suffix_score
 
For example, if sub = "nothing", prefix = "bruno", and suffix = "ingenious":
prefix_score = 2 because sub = "nothing" and prefix = "bruno" both have a substring, "no", that is common to the beginning of sub and the end of prefix, and "no" has length n = 2.
suffix_score = 3 because sub = "nothing" and suffix = "ingenious" both have a substring, "ing", that is common to the end of sub and the beginning of suffix, and "ing" has length n = 3.
text_score = prefix_score + suffix_score = 2 + 3 = 5
 
Complete the calculateScore function in the editor below. It has three parameters:
A string, text.
A string, prefix.
A string, suffix.
The function must return a string denoting the non-empty substring of text having a maximal text_score. If there are multiple such substrings, choose the lexicographically smallest substring.
 
Input Format
Locked stub code in the editor reads the following input from stdin and passes it to the function:
The first line contains a string denoting text.
The second line contains a string denoting prefix.
The third line contains a string denoting suffix.
 
Constraints
text, prefix, and suffix contain lowercase English alphabetic letters (i.e., a through z) only.
1 ≤ |text|, |prefix|, |suffix| ≤ 50, where |s| denotes the number of characters in some string s.
It is guaranteed that there will always be a substring of text that, at minimum, matches either of the following:
One or more characters at the end of prefix.
One or more characters at the beginning of suffix.
 
Sample Input 0
nothing
bruno
ingenious
 
Sample Output 0
nothing
 
Explanation 0
This example is explained above.
 
Sample Input 1
ab
b
a
 
Sample Output 1
a
 
Explanation 1
Given text = "ab", our possible substrings are sub = "a", sub = "b", and sub = "ab".
sub = "a"
prefix = "b": The beginning of sub doesn't match the end of prefix, so prefix_score = 0.
suffix = "a": The last character of sub matches the first character of suffix, so suffix_score = 1.
text_score = prefix_score + suffix_score = 0 + 1 = 1
sub = "b"
prefix = "b": The first character of sub matches the last character of prefix, so prefix_score = 1.
suffix = "a": The end of sub doesn't match the beginning of suffix, so suffix_score = 0.
text_score = prefix_score + suffix_score = 1 + 0 = 1
sub = "ab"
prefix = "b": The beginning of sub doesn't match the end of prefix, so prefix_score = 0.
suffix = "a": The last character of sub matches the first character of suffix, so suffix_score = 1.
text_score = prefix_score + suffix_score = 0 + 1 = 1
 
All of these have a text_score of 1, so we return the lexicographically smallest one (i.e., "a").


calculateScore text p s = undefined

-}

import Data.List

substring y = [t | i <- inits y, t <- tails i, not $ null t]

shear a b = let
  ab = zip a b
  in map fst $ takeWhile (\(a,b) -> a==b) ab

align ::  String -> String  -> String
align a b =
  if shear a b == b
  then b
  else align a $ tail b

getPrefix pre sub = align sub pre

getSuffix suf sub = align suf sub

calculateScore :: String -> String -> String -> String
calculateScore text pre suf = let
  subs = substring text
  pres = map (getPrefix pre) subs 
  sufs = map (getSuffix suf) subs
  scores = map (\(a,b) -> length $ a++b) $ zip pres sufs
  maxScores = reverse . sort $ zip scores [0..]
  takeTop l = let
    t = fst $ head l
    in map snd $ takeWhile ((==t) . fst) l
  winners = map (subs !!) $ takeTop maxScores
  in head $ sort winners

