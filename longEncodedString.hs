{-
Consider a string consisting of lowercase English alphabetic letters (i.e., [a-z]) only. We use the following rules to encode all of its characters into string s:
a is encoded as 1, b is encoded as 2, c is encoded as 3, …, and i is encoded as 9.
j is encoded as 10#, k is encoded as 11#, l is encoded as 12#, …, and z is encoded as 26#.
If there are two or more consecutive occurrences of any character, then the character count is written within parentheses (i.e., (c), where c is an integer denoting the count of consecutive occurrences being encoded) immediately following the encoded character. For example, consider the following string encodings:
String "abzx" is encoded as s = "1226#24#".
String "aabccc" is encoded as s = "1(2)23(3)".
String "bajj" is encoded as s = "2110#(2)".
String "wwxyzwww" is encoded as s = "23#(2)24#25#26#23#(3)".
 
Complete the frequency function in the editor below. It has one parameter: a string, s, that was encoded using the rules above and consists of digits (i.e., decimal integers from 0 to 9), # symbols, and parentheses. It must return an array of 26 integers where:
The element at index 0 denotes the frequency of character a in the original string.
The element at index 1 denotes the frequency of character b in the original string.
The element at index 2 denotes the frequency of character c in the original string.
…
The element at index 25 denotes the frequency of character z in the original string.
 
Input Format
Locked stub code in the editor reads encoded string s from stdin and passes it to the function.
 
Constraints
String s consists of decimal integers from 0 to 9, #'s, and ()'s only.
1 ≤ length of s ≤ 105
It is guaranteed that string s is a valid encoded string.
2 ≤ c ≤ 104, where c is a parenthetical count of consecutive occurrences of an encoded character.
 
Output Format
The function must return an array of 26 integers denoting the respective frequencies of each character (i.e., a through z) in the decoded string. This is printed to stdout by locked stub code in the editor.
 
Sample Input 0
1226#24#
 
Sample Output 0
1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 1
 
Explanation 0

 
Sample Input 1
1(2)23(3)
 
Sample Output 1
2 1 3 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
 
Explanation 1

 
Sample Input 2
2110#(2)
 
Sample Output 2
1 1 0 0 0 0 0 0 0 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
 
Explanation 2

 
Sample Input 3
23#(2)24#25#26#23#(3)
 
Sample Output 3
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 5 1 1 1

double = do
  n <- numbers
  char '#'
  
  
encoded :: Parser (Int,Int)
try (string "camel") <|> (string "cat")

repeated :: Parser (Int,Int)
repeated = do
  en <- encoded
  char '('
  r <- number
  char ')'
  let rd read :: String -> Int
  return fmap ((+) . rd) en

single :: Parser Int
single = let
  rd = read :: String -> Int
  in rd $ oneOf "123456789"

-}

import Control.Monad
import Text.Parsec
import Data.Array
import Data.List
import Data.Foldable (toList)
import Control.Applicative hiding ((<|>))
import Data.Array.Base (unsafeAt)

type Parser = Parsec String ()

ai :: Parser String 
ai = do
  n <- oneOf "123456789"
  return $ n:[]

jz :: Parser String
jz = do
  n <- many1 digit
  char '#'
  return n

az :: Parser (Int,Int)
az = do
  l <- try jz <|> ai
  let rd = read :: String -> Int
  return (rd l,1)

repeated :: Parser (Int,Int)
repeated = do
  en <- az
  char '('
  r <- many1 digit
  char ')'
  let rd = read :: String -> Int
  return (fst en, rd r)

letters = many1 $ try repeated <|> az

items arr = [ unsafeAt arr i
                | i <- [0 .. rangeSize(bounds arr)-1] ]
       
frequency s = let
  arr = parse letters "" s
  hist = fmap (items . (accumArray (+) 0 (1,26))) arr
  agg x y = x ++ y ++ " "
  output x = foldl' agg "" $ map show x
  in either (const "Invalid") output hist
  
