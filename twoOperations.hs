{-
You are given only two operations, ADD_1 and MULTIPLY_2. You start from 0 and using the two operations reach a number N. Find the least number of operations needed to do this.

Input Format: 
The first line will contain an integer T that gives the number of test cases. There will be one integer N on each of the following T lines.

Output Format
Print the minimum number of operations needed in each line corresponding to each test case.

Constraints
T ≤ 10,000
N ≤ 1016

Sample Input
2
5
3

Sample Output
4
3
Explanation
Case1: To reach 5 from 0. We do ADD1 then Multiply2 two times and then add 1 again , total 4 steps
Case2: To reach 3 from 0. We can either do Add 1 3 times or Add 1 , multiply 2 and again add 1
-}

minOps :: Int -> Int -> Int
minOps count 0 = count
minOps count value | value `mod` 2 == 0 = minOps (count + 1) (value `div` 2)
                   | otherwise = minOps (count + 1) (value - 1)

minOperations :: [String] -> [String]
minOperations nvalues = map show $ map (minOps 0) $ map read nvalues
