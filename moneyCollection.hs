{-
Julia is collecting money from n classmates for a trip. Each classmate is assigned a unique ID number from 1 to n, and each classmate i is prepared to donate exactly i dollars to the trip fund. For example, classmate 1 will donate 1 dollar, classmate 3 will donate 3 dollars, and so on. Julia plans to visit each classmate in order (i.e., from 1 to n) to collect their money, but she may also refuse to take their money because she is superstitious and does not ever want the current sum of collected money to be equal to k. Given n and k, what is the maximum amount of money she can collect?
 
Complete the maxMoney function in the editor below. It has two parameters:
An integer, n, denoting the number of classmates.
An integer, k, denoting Julia's unlucky number.
The function must return an integer denoting the maximum amount of money Julia can collect by visiting her classmates in order of sequential ID number and ensuring that the current sum of money collected is never equal to k. As the answer could be large, return answer % (109 + 7).
 
Input Format
Locked stub code in the editor reads the following input from stdin and passes it to the function:
The first line contains an integer, n, denoting the number of classmates.
The second line contains an integer, k, denoting Julia's unlucky number.
 
Constraints
1 ≤ n ≤ 2 × 109
1 ≤ k ≤ 4 × 1015
 
Output Format
The function must return an integer denoting the maximal amount of money Julia can collect by visiting her classmates in order of sequential ID number and ensuring that the current sum of money collected is never equal to k. As the answer could be large, return answer % (109 + 7). This is printed to stdout by locked stub code in the editor.
 
Sample Input 0
2
2
 
Sample Output 0
3
 
Explanation 0
Julia visits the following sequence of n = 2 classmates:
Julia collects 1 dollar from classmate 1 to get sum = 1.
Julia collects 2 dollars from classmate 2 to get sum = 1 + 2 = 3; observe that she collected a maximal amount of money and avoided having exactly k = 2 dollars.
 
Sample Input 1
2
1
 
Sample Output 1
2
 
Explanation 1
Julia visits the following sequence of n = 2 classmates:
Julia will not collect 1 dollar from classmate 1 because k = 1 and she refuses to have a sum ≡ k at any time.
Julia moves on and collects 2 dollars from classmate 2 to get sum = 0 + 2 = 2.
 
Sample Input 2
3
3
 
Sample Output 2
5
 
Explanation 2
Julia must skip some classmate because collecting from all her classmates will result in a sum ≡ k = 3 when she collects from the second classmate. There are two ways for her to visit all n = 3 classmates:
She can collect 1 dollar from classmate 1 to get sum = 1. Next, she can refuse to collect 2 dollars from classmate 2 to avoid having a sum equal to k. Next, she can collect 3 dollars from classmate 3 to get sum = 1 + 3 = 4.
She can refuse to collect 1 dollar from classmate 1, meaning that sum = 0. Next, she can collect 2 dollars from classmate 2 to get sum = 0 + 2 = 2. Next, she can collect 3 dollars from classmate 3 to get sum = 2 + 3 = 5.
Because we want the maximum amount of money that Julia can collect from her sequentially-numbered classmates without ever having a sum equal to k, we return 5 as our answer.
-}

isTriangle :: Integer -> Bool
isTriangle k = let
  smallest = dropWhile (< k) $ scanl (+) 0 [1,2..]
  in k == head smallest

maxMoney :: Int -> Integer -> Integer 
maxMoney n k = let
  n' = toInteger n
  s = sum [1..n'] :: Integer
  out = if isTriangle k then s-1 else s
  in mod out 1000000007

