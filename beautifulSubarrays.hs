{-
Given an array, a, of n distinct positive integers, we define the following:
Subarray a[i..j] contains elements a[i], a[i + 1], a[i + 2], …, a[j - 1], and a[j].
Two subarrays, a[i1..j1] and a[i2..j2] are considered to be distinct if i1 ≠ i2 or j1 ≠ j2.
 
We consider the subarray a[i..j] (where 0 ≤ i < n and j ≤ i < n) to be beautiful if it contains exactly m odd elements.
 
Complete the beautifulSubarrays function in the editor below. It has two parameters:
An array, a, of n positive distinct integers.
An integer, m, denoting the number of odd elements in a beautiful subarray.
The function must return a long integer denoting the total number of distinct beautiful subarrays.
 
Input Format
Locked stub code in the editor reads the following input from stdin and passes it to the function:
The first line contains an integer, n, denoting the number of elements in the array, a.
Each line i of the next n subsequent lines contains an integer describing ai.
The last line contains an integer, m.
 
Constraints
1 ≤ n ≤ 2 × 105
1 ≤ a[i] ≤ 109, where 0 ≤ i < n.
The array consists of distinct positive integers.
0 ≤ m ≤ 2 × 105
 
Output Format
The function must return a long integer denoting the total number of distinct beautiful subarrays. This is printed to stdout by locked stub code in the editor.
 
Sample Input 0
4
2
5
4
9
1
 
Sample Output 0
6
 
Explanation 0
Array a = [2, 5, 4, 9] has six distinct beautiful subarrays with exactly m = 1 odd elements:
a[1..1] = [5]
a[3..3] = [9]
a[0..1] = [2, 5]
a[1..2] = [5, 4]
a[2..3] = [4, 9]
a[0..2] = [2, 5, 4]
Thus, the function returns 6 as the answer.
 
Sample Input 1
4
2
5
4
9
2
 
Sample Output 1
2
 
Explanation 1
Array a = [2, 5, 4, 9] has two distinct beautiful subarrays with exactly m = 2 odd elements:
a[1..3] = [5, 4, 9]
a[0..3] = [2, 5, 4, 9]
Thus, the function returns 2 as the answer.
 
Sample Input 2
4
2
5
4
9
3
 
Sample Output 2
0
 
Explanation 2
Array a = [2, 5, 4, 9] has no distinct beautiful subarrays with exactly m = 3 odd elements, so the function returns 0 as the answer.
-}

import Data.List

-- note that this is brute force and only passes 5/14 tests
beautifulSubarrays :: [Int] -> Int -> Int
beautifulSubarrays a m = length $ filter (\l -> length l == m) subarrays
  where subarrays = map (filter odd) $ tail $ filter (flip isInfixOf $ numbers') $ subsequences numbers'
        numbers' = map toInteger a

