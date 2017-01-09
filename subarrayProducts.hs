{-
We define a subarray of an array, numbers, to be a contiguous block of numbers' elements having a length that is less than or equal to the length of the numbers array. For example, the subarrays of numbers = [1, 2, 3] are [1], [2], [3], [1, 2], [2, 3], and [1, 2, 3]. If we were to then find the products for each respective subarray, they would be 1, 2, 3, 1 × 2 = 2, 2 × 3 = 6, and 1 × 2 × 3 = 6.
 
Complete the count function in the editor below. It has two parameters:
An array of n integers, numbers.
An integer, k.
The function must return a long integer denoting the total number of subarrays where the product of the subarray's elements are less than k.
 
Input Format
Locked stub code in the editor reads the following input from stdin and passes it to the function:
The first line contains an integer, n, denoting the size of the numbers array.
Each line i of the n subsequent lines (where 0 ≤ i < n) contains an integer describing numbersi.
The last line contains a single integer denoting k.
 
Constraints
1 ≤ n ≤ 5 × 105
1 ≤ numbersi ≤ 102, where numbersi is the ith element of the numbers array.
1 ≤ k ≤ 106
 
Output Format
The function must return a long integer denoting the total number of subarrays having products less than k. This is printed to stdout by locked stub code in the editor.
 
Sample Input 0
3
1
2
3
4
 
Sample Output 0
4
 
Explanation 0
numbers = [1, 2, 3]. We have the following 6 subarrays:
[1] with a product of 1.
[2] with a product of 2.
[3] with a product of 3.
[1, 2] with a product of 1 × 2 = 2.
[2, 3] with a product of 2 × 3 = 6.
[1, 2, 3] with a product of 1 × 2 × 3 = 6.
 
The only subarrays having products less than k = 4 are [1], [2], [3], and [1, 2], so the function returns 4.
 
Sample Input 1
3
1
2
3
7
 
Sample Output 1
6
 
Explanation 1
numbers = [1, 2, 3]. We have the following 6 subarrays:
[1] with a product of 1.
[2] with a product of 2.
[3] with a product of 3.
[1, 2] with a product of 1 × 2 = 2.
[2, 3] with a product of 2 × 3 = 6.
[1, 2, 3] with a product of 1 × 2 × 3 = 6.
 
All six of the above subarrays have products less than k = 7, so the function returns 6.
-}
import Data.List

subseqs ls = [t | i <- inits ls, t <- tails i, not $ null t]
-- note that this is brute force and only passes 3/10 tests. use subseqs ^
count numbers k = length $ filter (<k') subarrays
  where subarrays =  map product $ tail $ filter (flip isInfixOf $ numbers') $ subsequences numbers'
        numbers' = map toInteger numbers
        k' = toInteger k
