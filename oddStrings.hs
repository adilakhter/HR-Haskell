{-
Problem Statement
We have an array, A, containing K strings. Each string si (where A[i] = si and 0 ≤ i ≤ K - 1) is composed of lowercase English letters (a-z) and has s_i_length characters (where 1 ≤ s_i_length ≤ 105).
 
Consider each si as an indexed array of characters in the inclusive range from 0 to s_i_length - 1. Given an integer, M, we perform the following calculation for each string:
 
calculation(si) = (ASCII value of si[0])M × (ASCII value of si[1])M × … × (ASCII value of si[s_i_length - 1])M
 
For example: if s = "ab" and M=2, we get (97)2 × (98)2 = 90,364,036. Recall that 97 and 98 are the respective ASCII values for a and b.
 
Sum the results of your calculations for each si in A, and print whether this total sum is EVEN or ODD.
 
Input Format
The first line contains a single integer, T (the number of test cases); the subsequent 2T lines describe each test case over two lines.
The first line of each test case contains two space separated integers, M (the exponent used in our calculation) and K (the number of strings in array A), respectively.
The second line of each test case contains K space-separated strings describing array A.
 
Constraints
1 ≤ T ≤ 50, where T is the number of test cases.
2 ≤ K ≤ 20, where K is the number of strings in some test case Ti.
2 ≤ M ≤ 109, where M is the given integer used in our calculation.
1 ≤ s_i_length ≤ 105, where s_i_length is the length of the string in A[i].
 
Output Format
On a new line for each of the T test cases, print EVEN if the sum of your calculated results is even or ODD if the sum of your calculated results is odd.
 
Sample Input 0
2
50 3
aceace ceceaa abdbdbdbakjkljhkjh
47 3
azbde abcher acegk
 
Sample Output 0
EVEN
ODD
-}

import Control.Monad
import Text.Regex (splitRegex, mkRegex)
import Data.Char
import Data.Bits

tokenize :: String -> [String]
tokenize = splitRegex (mkRegex "[[:space:],]")

getInts :: String -> [Int]
getInts l = map (\x -> read x :: Int) $ tokenize l

getStrings :: String -> [String]
getStrings = tokenize

getTuple :: IO (String, String)
getTuple = liftM2 (,) getLine getLine

convert :: Int -> String -> Bool
convert exp s = foldr (&&) True $ map (odd . (flip (^) exp) . (flip mod 2) . ord) s

output :: Bool -> String
output True = "ODD"
output False = "EVEN"
  
main :: IO ()
main = do
  n <- readLn :: IO Int
  tups <- replicateM n getTuple
  let inputs = fmap (getStrings . snd) tups
      args = fmap (head . getInts . fst) tups
      app (arg,input) = map (convert arg) input
      outputs = map app $ zip args inputs
  mapM_ (putStrLn . output) $ map (foldr xor False) outputs
