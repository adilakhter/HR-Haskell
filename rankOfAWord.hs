{-
The rank of a word (a string of characters) is the position that the word occupies in the lexical ordering of all permutations of the characters in that word. For example 
 
rank(bac) = 2, because the lexical ordering of its permutations are [abc,acb,bac,bca,cab,cba]. We use 0-based indexing.
 
There are multiple test cases. In each test case, you are given a word. You have to find the rank of that word among the complete set of permutations. The words in the test cases contain lowercase letters [a-z] only. Most words will contain repeated characters, so take that into consideration.

Input Format
The first line of the input contains a single integer, T, which is the number of test cases in the list. 
This is followed by T lines each containing a single lowercase word W.

Output Format
For each test case, print an integer on a separate line which is the rank of the given word. As the rank can be very high, print it modulo (109 + 7)

Stubs:
There is functioning code already present in the development environment to handle input and output in the above formats. Your task is to complete the body of the function get_ranks.

Constraints:
1 ≤ T ≤ 103
1 ≤  length(W) ≤102

Sample Input #1:
3
bac
aaa
abba
 
Sample Output #1:
2
0
2
 
Explanation #1:
In the first case, the permutations are abc,acb,bac,bca,cab,cba, in which counting from 0, bac occupies position 2.
In the second case, there is only one permutation aaa and hence the rank is 0.
In the third case, we get aabb and abab before abba, and hence the rank is 2.

Note for finding modulo inverse - 
You may need to find (x/a) mod n. This is equivalent to xa-1 mod n, where a-1 is the modulo multiplicative inverse.
If n is relatively prime to a, then the inverse modulo can be found with using Newton's algorithm is as follows. 
Source : Wikipedia
function inverse(a, n)
    t := 0;     newt := 1;    
    r := n;     newr := a;    
    while newr ≠ 0
        quotient := r div newr
        (t, newt) := (newt, t - quotient * newt) 
        (r, newr) := (newr, r - quotient * newr)
    if r > 1 then return "a is not invertible"
    if t < 0 then t := t + n
    return t
You may or may not need the above depending on your environment and your ability to codeyour own version.
Haskell
-}

import Data.List
import Data.Maybe

-- slow. 1/10 tests passing
getRank word = 
  let perms = nub $ permutations word
      perms' = sort perms
      index = fromMaybe 0 $ findIndex (==word) perms'
  in index
get_ranks words = map (show . getRank) words

--displayList lst out = sequence_ [hPutStrLn out a | a <- lst]
