{-
Sid loves to read short stories. Being a Computer Science student, he decides to do some frequency analysis on his favorite reading material. For each data point, chooses a string of length a from one book, and a string of length b from a second book. The strings' lengths differ by no more than 1.
|a-b|≤1, where |x| represents the absolute value function.
 
The frequency analysis consists of checking how far the strings are from being anagrams of one another. Your challenge is to help him find the minimum number of characters of the first string he needs to change to make it an anagram of the second string.  He can neither add nor delete characters from the first string. Only replacement of the characters with new ones is allowed.

Input Format
The first line will contain an integer T representing the number of test cases. Each test case will contain a string having length (a+b) which will be concatenation of both the strings described in problem. The string will only contain small letters and without any spaces.

Output Format
An integer corresponding to each test case is printed in a different line i.e., the number of changes required for each test case. Print ‘-1’ if it is not possible.

Constraints
1 ≤ T ≤ 100
 1 ≤ a+b ≤ 10,000

Sample Input
5
aaabbb
ab
abc
mnop
xyyx
Sample Output
3
1
-1
2
0 

Explanation
In the five test cases
One string must be “aaa” and the other “bbb”. The lengths are a=3 and b=3, so the difference is less than 1. No characters are common between the strings, so all three must be changed.
One string must be “a” and the second “b”. The lengths are a=1 and b=1, so the difference is less than 1. One character must be changed to them the same.
Since the string lengths a and b must differ by no more than 1, the lengths are either a=1 and b=2 or a=2 and b=1. No sequence of substitutions will make the two  anagrams of one another.
One string must be “mn" and other be “op”. The length are a=2 and b=2, so the difference is less than 1. No characters are common between the strings, so both must be changed.
One string must be “xy” and the other be “yx”. The length are a=2 and b=2, so the difference is less than 1. No changes are needed because the second string is already an anagram of the first.
-}

import Control.Applicative
import Control.Monad
import qualified Data.List as L
import qualified Data.Map.Strict as M


type MultiSet a = M.Map a Int


fromList :: Ord a => [a] -> MultiSet a
fromList =
  L.foldl' (flip insert) M.empty
  

insert :: Ord a => a -> MultiSet a -> MultiSet a
insert el =
  M.insertWith (+) el 1


intersection :: Ord a => MultiSet a -> MultiSet a -> MultiSet a
intersection =
  M.intersectionWith min


length_ :: MultiSet a -> Int
length_ =
  M.foldl' (+) 0


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


main :: IO ()
main = do
  t <- (read :: String -> Int) <$> getLine
  replicateM_ t $ do
    str <- getLine
    print $ solve str
