{-
You are given an array A of size N. You are also given an Integer Q and you have to answer Q queries.
Each  query containing two integers x and y that work as follows:
 
find(int x, int y)
{
    if(x>y) return 1;
    ans = pow(A[x],find(x+1,y))
    return ans
}
Note : pow(a,b) = ab.
Array index starts at 1.
No two consecutive entries in the array will be zero.
 
Constraints
2 ≤ N ≤ 105
2 ≤ Q ≤ 105
1 ≤ x,y ≤ N
x ≤ y
 
Input Format
The first line contains an integer N.
The next line contains N space separated single-digit integers (whole numbers between 0 and 9).
The third line contains a positive integer Q, denoting the number of queries to follow.
Each of the subsequent Q lines contains two positive integers x and y separated by a single space.
 
Output Format
For each query, print the  "Even"  if the value returned is even, otherwise print "Odd " without quotes.
 
Sample Input #00
3
3 2 7
2
1 2
2 3
 
Sample Output #00
Odd
Even
 
Explanation #00
find(1,2) = 32 = 9, which is odd.
find(2,3) = 27 = 128, which is even.
-}

import qualified Data.Vector as V
import Control.Monad
import Text.Regex (splitRegex, mkRegex)


tokenize :: String -> [String]
tokenize = splitRegex (mkRegex "[[:space:],]")

getInts :: IO [Int]
getInts = getLine >>= (\x -> return $ fmap (\x -> read x :: Int) $ tokenize x)

getTuple :: IO (String, String)
getTuple = liftM2 (,) getLine getLine

find' vec x y | x > y = 1
              | otherwise = (vec V.! (x-1)) ^ (find' vec (x+1) y)

output i | even i = "Even"
         | otherwise = "Odd"


main :: IO ()
main = do
  n <- readLn :: IO Int
  arr <- getInts
  q <- readLn :: IO Int
  queries <- replicateM q getInts
  let vec = V.fromList arr
      outputs = fmap (\l -> output $ find' vec (l!!0) (l!!1)) queries
  mapM_ putStrLn $ outputs
