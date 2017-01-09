{-
We define a subarray of array a to be a contiguous block of a's elements having a length that is less than or equal to the length of array a. For example, the subarrays of array a = [1, 2, 3] are [1], [2], [1, 2], [2, 3], and [1, 2, 3]. Now, let's say we have an integer, k = 3. The subarrays of a having elements that sum to a number ≤ k are [1], [2], and [1, 2]. The longest of these subarrays is [1, 2], which has a length of 2.
 
Complete the maxLength function in the editor. It has 2 parameters:
An array of integers, a.
An integer, k.
The function must return the length of the longest subarray having elements that sum to a number less than or equal to k. You cannot reorder the array's elements.
 
Input Format
Locked stub code in the editor reads the following input from stdin and passes it to the function:
The first line contains a single integer, n, denoting the number of elements in array a.
Each line i of the n subsequent lines (where 0 ≤ i < n) contains an integer describing element i in array a.
The last line contains an integer, k.
 
Constraints
1 ≤ n ≤ 105
1 ≤ a[i] ≤ 103
1 ≤ k ≤ 109
 
Output Format
The function must return the length of the longest subarray having a sum less than or equal to k. This is printed to stdout by locked stub code in the editor.
 
Sample Input 0
3
1
2
3
4
 
Sample Output 0
2
 
Explanation 0
The subarrays of [1, 2, 3] having elements that sum to a number ≤ (k = 4) are [1], [2], [3], and [1, 2]. The longest of these is [1, 2], which has a length of 2. Thus, we return 2 as our answer.
 
Sample Input 1
4
3
1
2
1
4
 
Sample Output 1
3
 
Explanation 1
The subarrays of [3, 1, 2, 1] having elements that sum to a number ≤ (k = 4) are [3], [1], [2], [1], [3, 1], [1, 2], [2, 1], and [1, 2, 1]. The longest of these is [1, 2, 1], which has a length of 3. Thus, we return 3 as our answer.

if sum new <= k -> 

if a' > k
  then (

agg case soFar of
  (a,old) | sum (next:old) <= k && length (next:old) > a -> (length (next:old),next:old)

where new = nextElt : (fst soFar)
      k' = length new
      jet = tail $ scanl (+) 0 (reverse new)
-}


agg :: Int -> (Int,[Int]) -> Int -> (Int,[Int])
agg k (a,old) next
  | sum (next:old) <= k && length (next:old) > a = (a+1,next:old)
  | sum (next:old) <= k && length (next:old) <= a = (a,next:old)
  | sum (next:old) > k = (a, old'')
    where jet = tail $ scanl (+) 0 old'
          out = length (takeWhile (<= k) jet)
          old' = next:old
          old'' = take out old'

maxLength' :: [Int] -> Int -> (Int,[Int])
maxLength' a k = foldl (agg k) (0,[]) a

maxLength a k = fst $ maxLength' a k
