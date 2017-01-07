{-
We define an anagram to be a word whose characters can be rearranged to create another word. Given two strings, we want to know the minimum number of characters already in either string that we must modify to make the two strings anagrams; if it's not possible to make the two strings anagrams, we consider this number to be -1. For example:
tea and ate are anagrams, so we would need to modify a minimum of 0 characters.
tea and toe are not anagrams, but we can modify a minimum of 1 character in either string to make them anagrams.
act and acts are not anagrams and cannot be converted to anagrams because they contain different numbers of characters, so the minimum number of characters to modify is -1.
 
Complete the function in the editor below. It has two parameters:
An array of n strings, a.
An array of n strings, b.
The function must return an array of integers where each element i denotes the minimum number of characters you must modify to make ai and bi anagrams; if it's not possible to modify the existing characters in ai and bi to make them anagrams, element i should be -1 instead.
 
Note: You can only modify existing characters in the strings, you cannot delete or append characters to change a string's length.
 
Input Format
Locked stub code in the editor reads the following input from stdin and passes it to the function:
The first line contains an integer, n, denoting the number of elements in a.
Each line i of the n subsequent lines contains an integer describing ai.
The next line contains an integer, n, denoting the number of elements in b.
Each line i of the n subsequent lines contains an integer describing bi.
 
Constraints
Each string consists of lowercase English alphabetic letters (i.e., a to z).
1 ≤ n ≤ 100
It is guaranteed that a and b contain the same number of elements.
0 ≤ length of ai, length of bi ≤ 104
1 ≤ length of ai + length of bi ≤ 104
 
Output Format
The function must return an array of integers where each element i denotes the minimum number of characters you must modify to make ai and bi anagrams; if it's not possible to modify the existing characters in ai and bi to make them anagrams, element i should be -1 instead. This is printed to stdout by locked stub code in the editor.
 
Sample Input 0
5
a
jk
abb
mn
abc
5
bb
kj
bbc
op
def
 
Sample Output 0
-1
0
1
2
3
 
Explanation 0
Given a = [a, jk, abb, mn, abc] and b = [bb, kj, bbc, op, def], we perform the following n = 5 calculations:
Index 0: a and bb cannot be anagrams because they contain different numbers of characters, so we return -1 at this index.
Index 1: jk and kj are already anagrams because they both contain the same characters at the same frequencies, so we return 0 at this index.
Index 2: abb and bbc differ by a minimum of one character, so we return 1 at this index.
Index 3: mn and op differ by a minimum of two characters, so we return 2 at this index.
Index 4: abc and def differ by a minimum of three characters, so we return 3 at this index.
After checking each pair of strings, we return the array [-1, 0, 1, 2, 3] as our answer.
-}

import Control.Applicative
import Control.Monad
import qualified Data.List as L
import qualified Data.Map.Strict as M


type MultiSet a = M.Map a Int


fromList :: Ord a => [a] -> MultiSet a
fromList = L.foldl' (flip insert) M.empty
  
insert :: Ord a => a -> MultiSet a -> MultiSet a
insert el = M.insertWith (+) el 1

intersection :: Ord a => MultiSet a -> MultiSet a -> MultiSet a
intersection = M.intersectionWith min

length_ :: MultiSet a -> Int
length_ = M.foldl' (+) 0

solve :: String -> Int
solve str =
  let
    n = length str
    result
      | odd n = -1
      | otherwise =
        let
          n2 = n `div` 2
          (str1, str2) = L.splitAt n2 str
          s = intersection (fromList str1) (fromList str2)
        in
          length str1 - length_ s
  in result

getMinimumDifference :: [String] -> [String] -> [String]
getMinimumDifference a b =  map (show . solve . uncurry (++)) $ zip a b

a = ["a","jk","abb","mn","abc"]
b = ["bb","kj","bbc","op","def"]

main = return $ getMinimumDifference a b
