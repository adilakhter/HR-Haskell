{-
We define a subarray of size x in an n-element array to be the contiguous block of elements in the inclusive range from index i to index j, where j − i + 1 = x and 0 ≤ i ≤ j < n. For example, given array [8, 2, 4], the subarrays of size x = 2 would be [8, 2] and [2, 4].
 
Complete the segment function in your editor. It has 2 parameters:
An integer, x.
An array of n integers, arr.
 
Your function must find the minimum value for each subarray of size x in array arr. After finding the minimum value for each x-element subarray of arr, it must return an integer denoting the maximum of these minimums.
 
Input Format
The locked stub code in your editor reads the following input from stdin and passes it to your function:
The first line contains an integer, x, denoting the size of your subarrays.
The second line contains an integer, n, denoting the size of array arr.
Each line i of the n subsequent lines (where 0 ≤ i < n) contains an integer describing element i in arr.
 
Constraints
1 ≤ n ≤ 106
1 ≤ arri ≤ 109, where 0 ≤ i < n
1 ≤ x ≤ 105
 
Output Format
Your function must return an integer denoting the maximum of the respective minimum values for each x-element subarray in arr. This is printed to stdout by the locked stub code in your editor.
 
Sample Input 1
The following arguments are passed to your function:
x = 1
arr = {1, 2, 3, 1, 2}
 
Sample Output 1
3
 
Explanation 1
The subarrays of size x = 1 are {1}, {2}, {3}, {1}, and {2}. Because each subarray only contains 1 element, each value is minimal with respect to the subarray it's in. We return the maximum of these values, which is 3.
 
Sample Input 2
The following arguments are passed to your function:
x = 2
arr = {1, 1, 1}
 
Sample Output 2
1
 
Explanation 2
The subarrays of size x = 2 are {1, 1} and {1, 1}. The minimum value for both subarrays is 1. We return the maximum of two 1's, which is 1.
 
Sample Input 3
The following arguments are passed to your function:
x = 3
arr = {2, 5, 4, 6, 8}
 
Sample Output 3
4
 
Explanation 3
The subarrays of size x = 3 are {2, 5, 4}, {5, 4, 6}, and {4, 6, 8}. The respective minimum value for the three subarrays are 2, 4, and 4. We return the maximum of these values, which is 4.
-}

import Data.List

-- this problem is broken on HR
subsequence :: Int -> [Int] -> [[Int]]
subsequence x arr  = filter ((==x) . length) $ filter (flip isInfixOf $ arr) $ subsequences arr
getMin = foldl min (maxBound :: Int)

getMax = foldl max (minBound :: Int)

segment x arr = getMax $ map getMin $ subsequence x arr 
