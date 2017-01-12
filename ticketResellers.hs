{-
There is a row of n ticket resellers outside of a hockey game, and each reseller is numbered from 0 to n−1. Each reseller i (where 0 ≤ i < n) has ai tickets available for sale. They band together and price their tickets according to the following rule: if reseller i has ai tickets available for sale, reseller i will sell that ticket for $ai; this means that the price of each ticket sold by reseller i is equal to the number of tickets that reseller i had in his possession (i.e., ai) at the time of the sale. For example, if reseller i = 0 has a0 = d tickets for sale, the first ticket sold by reseller 0 will be $d, the second ticket sold by reseller 0 will be $(d−1), and so on. Reseller 0's prices have no effect on the ticket prices of the other resellers.
 
Complete the maximumAmount function in your editor. It has 2 parameters:
An array of n integers, a, where the value of each element ai is the number of tickets available for sale by reseller i (where 0 ≤ i < n).
A long integer, k, the number of tickets you must find the maximum possible revenue for.
It must return a long integer denoting the maximum amount of money that the group can earn from the first k tickets sold.
 
Input Format
The locked stub code in your editor reads the following input from stdin and passes it to your function:
The first line contains an integer, n (the number of resellers). Each line i of the n subsequent lines contains an integer describing the respective numbers of tickets for sale by each reseller i (where 0 ≤ i < n). The very last line contains a long integer, k (the number of tickets to find the maximum revenue for).
 
Constraints
1 ≤ n ≤ 105
1 ≤ ai ≤ 105, where 0 ≤ i < n
1 ≤ k ≤ a0 + a1 + … + an−1
 
Output Format
Your function must return a single long integer denoting the maximum amount of money the group of resellers will make from selling the first k tickets using this pricing scheme. This is printed to stdout by the locked stub code in your editor.
 
Sample Input 0
2
2
5
4
 
Sample Output 0
14
 
Sample Input 1
5
2 
8 
4 
10 
6
20
 
Sample Output 0
110
 
Explanation
Sample Case 0:
The resellers maximize their revenue by having reseller a1 sell all k = 4 tickets. The first ticket sold is $5, the second ticket is $4, the third ticket is $3, and the fourth ticket is $2. This gives us a maximum sales number of $14, so we print 14.
-}

import Data.List
import Data.Ord

import Data.Traversable
import Data.Maybe
import qualified Data.Vector as V

-- list version
maxi xs = maximumBy (comparing fst) (zip xs [0..])

agg (total,a') li = (total + ai, a'') where
   (ai, i) = maxi a'
   a'' = (take (i) a') ++ [ai -1] ++ (drop (i+1) a')
   
-- 7/20 test cases passed
maximumAmountList a k = fst $ foldl' agg (0,a) [1 | i <- [1..k]]

  
-- vector version

-- 8/20 test cases passed
maximumAmount a k = maximumAmount' (V.fromList a) k 0

maximumAmount' a 0 acc = acc
maximumAmount' a k acc = maximumAmount' a' (k-1) acc'
  where max = V.maximum a
        i = fromMaybe 0 $ V.findIndex (==max) a
        a' = a V.// [(i,max-1)]
        acc' = acc + max
