{-
Julia calls a string magical if the following conditions are satisfied:
Each letter ∈ {a, e, i, o, u}.
"a" must only be followed by "e".
"e" must only be followed by "a" or "i".
"i" must only be followed by "a", "e", "o", or "u".
"o" must only be followed by "i" or "u".
"u" must only be followed by "a".
 
Complete the magicalStrings function in your editor. It has 1 parameter: an integer, n, denoting the desired length of a magical string. It must return an integer denoting the number of magical strings of length n, modulo (109 + 7).
 
Input Format
The locked stub code in your editor reads the following input from stdin and passes it to your function:
The first line contains an integer, n, denoting the desired length of a magical string.
 
Constraints
0 < n < 105
 
Output Format
Your function must return an integer denoting the number of magical strings of length n, modulo (109 + 7). This is printed to stdout by the locked stub code in your editor.
 
Sample Input 0
1
 
Sample Output 0
5
 
Sample Input 1
2
 
Sample Output 1
10
 
Sample Input 2
3
 
Sample Output 2
19
 
Explanation
Sample Case 0:
There 5 magical strings: {"a", "e", "i", "o", "u"}.
 
Sample Case 1:
There 10 magical strings: {"ae", "ea", "ei", "ia", "ie", "io", "iu", "oi", "ou", "ua"}.
 
Sample Case 2:
There 19 magical strings: {"aea", "aei", "eae", "eia", "eie", "eio", "eiu", "iae", "iea", "iei", "ioi", "iou", "iua", "oia", "oie", "oio", "oiu", "oua", "uae"}.
 -}

-- "a" must only be followed by "e".
-- "e" must only be followed by "a" or "i".
-- "i" must only be followed by "a", "e", "o", or "u".
-- "o" must only be followed by "i" or "u".
-- "u" must only be followed by "a".

import Debug.Trace

options :: Int -> Char -> Int
--options l c | trace (show l ++ " " ++ show c) False = undefined
options l c = if l == 1 then 1 else case c of
  'a' -> options (l-1) 'e'
  'e' -> sum $ map (options (l-1)) "ai"
  'i' -> sum $ map (options (l-1)) "aeou"
  'o' -> sum $ map (options (l-1)) "iu"
  'u' -> sum $ map (options (l-1)) "a"
  
magicalStrings n = sum $ map (options n) "aeiou"

