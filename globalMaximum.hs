{-
Consider an array, a, of n distinct positive integers where the elements are sorted in ascending order. We want to find all the subsequences of a consisting of exactly m elements. For example, given array a = [1, 2, 3, 4], the subsequences consisting of m = 3 elements are {1, 2, 3}, {1, 2, 4}, {1, 3, 4}, and {2, 3, 4}. Once we have all of the m-element subsequences, we find the value of globalMaximum using the following pseudocode:
 
globalMaximum = 0

for each subsequence, s, consisting of m elements {
    currentMinimum = 1018

    for each (x, y) pair of elements in subsequence s {
        absoluteDifference = abs(x - y)

        if absoluteDifference < currentMinimum {
            currentMinimum = absoluteDifference
        }
    }
	
    if currentMinimum > globalMaximum {
        globalMaximum = currentMinimum
    }
}
 
Complete the findMaximum function in the editor below. It has two parameters:
An array, a, of n distinct positive integers that are sorted in ascending order.
An integer, m, denoting the number number of elements that must be in each subsequence.
The function must return an integer denoting the value of globalMaximum as defined in the pseudocode above.
 
Input Format
Locked stub code in the editor reads the following input from stdin and passes it to the function:
The first line contains an integer, n, denoting the number of elements in a.
Each line i of the n subsequent lines (where 0 ≤ i < n) contains an integer describing ai.
The last line contains an integer, m.
 
Constraints
2 ≤ n ≤ 105
1 ≤ ai ≤ 109, where 0 ≤ i < n.
The array consists of distinct positive integers sorted in ascending order.
2 ≤ m ≤ n
 
Output Format
The function must return an integer denoting the value of globalMaximum as defined in the given pseudocode. This is printed to stdout by locked stub code in the editor.
 
Sample Input 0
4
1
2
3
4
3
 
Sample Output 0
1
 
Explanation 0
The subsequences of array a = [1, 2, 3, 4] consisting of m = 3 elements are {1, 2, 3}, {1, 2, 4}, {1, 3, 4}, and {2, 3, 4}.
After the iteration on subsequence {1, 2, 3}, the value of globalMaximum is 1.
After the iteration on subsequence {1, 2, 4}, the value of globalMaximum is 1.
After the iteration on subsequence {1, 3, 4}, the value of globalMaximum is 1.
After the iteration on subsequence {2, 3, 4}, the value of globalMaximum is 1.
Thus, the function returns 1 as the answer.
 
Sample Input 1
4
1
2
3
4
2
 
Sample Output 1
3
 
Explanation 1
The subsequences of array a = [1, 2, 3, 4] consisting of m = 2 elements are {1, 2}, {1, 3}, {1, 4}, {2, 3}, {2, 4}, and {3, 4}.
After the iteration on the subsequence {1, 2}, the value of globalMaximum is 1.
After the iteration on the subsequence {1, 3}, the value of globalMaximum is 2.
After the iteration on the subsequence {1, 4}, the value of globalMaximum is 3.
After the iteration on the subsequence {2, 3}, the value of globalMaximum is 3.
After the iteration on the subsequence {2, 4}, the value of globalMaximum is 3.
After the iteration on the subsequence {3, 4}, the value of globalMaximum is 3.
Thus, the function returns 3 as the answer.
 
Sample Input 2
5
1
2
4
5
8
3
 
Sample Output 2
3
 
Explanation 2
The subsequences of array a = [1, 2, 4, 5, 8] consisting of m = 3 elements are {1, 2, 4}, {1, 2, 5}, {1, 2, 8}, {1, 4, 5}, {1, 4, 8}, {1, 5, 8}, {2, 4, 5}, {2, 4, 8}, {2, 5, 8}, and {4, 5, 8}.
After the iteration on the subsequence {1, 2, 4}, the value of globalMaximum is 1.
After the iteration on the subsequence {1, 2, 5}, the value of globalMaximum is 1.
After the iteration on the subsequence {1, 2, 8}, the value of globalMaximum is 1.
After the iteration on the subsequence {1, 4, 5}, the value of globalMaximum is 1.
After the iteration on the subsequence {1, 4, 8}, the value of globalMaximum is 3.
After the iteration on the subsequence {1, 5, 8}, the value of globalMaximum is 3.
After the iteration on the subsequence {2, 4, 5}, the value of globalMaximum is 3.
After the iteration on the subsequence {2, 4, 8}, the value of globalMaximum is 3.
After the iteration on the subsequence {2, 5, 8}, the value of globalMaximum is 3.
After the iteration on the subsequence {4, 5, 8}, the value of globalMaximum is 3.
Thus, the function returns 3 as the answer.
-}

import Data.List

subsequence :: [Int] -> Int -> [[Int]]
subsequence a m = filter ((==m) . length) $ subsequences a

pairs :: [a] -> [(a, a)]
pairs l = [(x,y) | (x:ys) <- tails l, y <- ys]

minAbs l = foldl min (maxBound :: Int) $ map (\(x,y) -> abs (x-y)) $ pairs l
  
findMaximum :: [Int] -> Int -> Int
findMaximum a m = foldl max 0 $ map minAbs $ subsequence a m

