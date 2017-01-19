{-
Two integers, x and y denote the magnitude of two desirable qualities in an entity. Entity A:x1,y1 is said to dominate entity B:x2,y2 if and only if both x1 > x2 and y1 > y2 . It possible for two entities to have no dominance between them.
 
For any set of entities, a member entity A is said to be non-dominatable if it is not dominated by any entity in that set.
 
Given a list of entities L, find the number of non-dominatable entities in the list.
 
Input Format
The first line of the input contains a single integer N, which is the number of entities in the list. 
N lines follow, each containing two integers x and y separated by a space. Each line describes one entity.
 
Output Format
A single integer which is the number of non-dominatable entities in the given list. 
 
Limits
1 ≤ N ≤ 106
1 ≤  x,y ≤ 232
 
Sample Test Cases:
 
Input #1
5
3 2
8 7
6 9
3 4
7 8
 
Output #1:
3
 
Explanation:
 
(8,7) , (6,9) , (7,8) cannot be dominated by any entity of the list. 
 
 
Input #2:
 
4
2 4
4 1
8 8
3 5
 
Output #2:
1
 
Explanation:
(8,8) dominates everything hence is the only non-dominatable entity.
-}


import Control.Monad
import Text.Regex (splitRegex, mkRegex)
import Data.List

data Entity = Entity Int Int deriving (Show, Eq)

instance Ord Entity where
  compare (Entity a b) (Entity c d) | a < c && b < d = LT
                                    | a > c && b > d = GT
                                    | otherwise = EQ
tokenize :: String -> [String]
tokenize = splitRegex (mkRegex "[[:space:],]")

getEntity :: IO Entity
getEntity = do
  x <- getLine
  let [a,b] = fmap (\x -> read x :: Int) $ tokenize x
  return $ Entity a b

main :: IO ()
main = do
  q <- readLn :: IO Int
  queries <- replicateM q getEntity
  let sorted = reverse $ sort queries
      big = head sorted
      output = takeWhile (>=big) sorted
  print $ length output
