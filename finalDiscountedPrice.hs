{-
A shopkeeper has a line of n items numbered from 0 to n−1. For each item i (where 0 ≤ i ≤ n−1), the shopkeeper gives a discount, di, which is the price of the first item to its right satisfying di ≤ pricei; if no such item satisfies this inequality, then the shopkeeper does not give a discount on the item at that index.
 
Complete the finalPrice function in your editor. It has 1 parameter: an array of integers, prices, containing the price for each of the shopkeeper's items. It must determine the cost to purchase all n items and take note of any items that are not discounted. The function should print the following two lines of output:
The first line should contain the cost of purchasing all n items.
The second line should contain the indices of any non-discounted items as a single line of space-separated values in increasing order.
 
Input Format
The locked stub code in your editor reads the following input from stdin and passes it to your function:
The first line contains an integer, n, denoting the number of items for sale.
Each line i of the n subsequent lines (where 0 ≤ i < n) contains an integer describing the price of item i.
 
Constraints
1 ≤ n ≤ 105
1 ≤ pricesi ≤ 105, where 0 ≤ i < n
 
Output Format
Your function must print the following two lines of output:
The first line should contain the cost of purchasing all n items.
The second line should contain the indices of any non-discounted items as a single line of space-separated values in increasing order.
 
Sample Input 1
6
5
1
3
4
6
2
 
Sample Output 1
14
1 5
 
Sample Input 2
5
1
3
3
2
5
 
Sample Output 2
9
0 3 4
 
Explanation
Sample Case 1:
n = 6
prices = {5, 1, 3, 4, 6, 2}
discount = {1, 0, 2, 2, 2, 0}, where discounti is the discount for pricesi.
The total cost for all items is: (5 − 1) + (1 − 0) + (3 − 2) + (4 − 2) + (6 − 2) + (2 − 0) = 14. There are no discounts for the items at indices 1 and 5.
 
Sample Case 2:
n = 5
prices = {1, 3, 3, 2, 5}
discount = {0, 3, 2, 0, 0}, where discounti is the discount for pricesi.
The total cost for all items is: (1 − 0) + (3 − 3) + (3 − 2) + (2 − 0) + (5 − 0) = 9. There are no discounts for the items at indices 0, 3, and 4.
 -}

import Data.List
import Control.Monad (replicateM)
import qualified Data.Vector as V
import Debug.Trace

agg :: (V.Vector Int, V.Vector (Int,Int), Int) -> Int -> (V.Vector Int, V.Vector (Int,Int), Int)
agg (discounts,memory,offset) next | trace (show memory) False = undefined
agg (discounts,memory,offset) next = let
  indices = V.toList $ V.map fst $ V.filter (\(i,a) -> a >= next) memory
  updates = zip indices [next,next..]
  discounts' = discounts V.// updates
  offset' = offset+1
  memory' =  V.snoc (V.filter (\(i,a) -> a < next) memory) (offset',next)
  in (discounts', memory', offset')

getDiscounts prices = let
  discounts = V.fromList [0 | i <- [1..length prices]]
  memory = V.fromList [(0,head prices)]
  (discounts', memory', offset') = foldl' agg (discounts,memory,0) $ tail prices
  price = sum prices - V.sum discounts'
  noDiscount = V.toList $ V.map fst memory'
  in (price, noDiscount)
  

finalPrice prices = do
  let (price,noDiscount) = getDiscounts prices
  putStrLn $ show price
  putStrLn $ unwords $ map show noDiscount

getInt = readLn :: IO Int
main = do
  n <- getInt
  prices <- replicateM n getInt
  finalPrice prices


