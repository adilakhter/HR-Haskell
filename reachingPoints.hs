{-
Jen coded a bot that takes a pair of integers coordinates, (x, y). Though the bot can move any number of times, it can only make the following two types of moves:
From location (x, y) to location (x + y, y).
From location (x, y) to location (x, x + y).
 
For example, if the bot starts at (1, 4), it can make the following sequence of moves: (1, 4) → (5, 4) → (5, 9) → (5, 14).
 
Complete the canReach function in the editor below. It has four integer parameters: x1, y1, x2, and y2. It must return the string Yes if it is possible for the bot to start at point (x1, y1) and reach point (x2, y2); otherwise, it must return the string No.
 
Input Format
Locked stub code in the editor reads the following input from stdin and passes it to the function:
The first line contains an integer denoting x1.
The second line contains an integer denoting y1.
The third line contains an integer denoting x2.
The fourth line contains an integer denoting y2.
 
Constraints
1 ≤ x1, y1, x2, y2 ≤ 1000
 
Output Format
The function must return a string denoting whether or not the bot can reach point (x2, y2) from point (x1, y1). If it is possible, return Yes; otherwise, return No. This is printed to stdout by locked stub code in the editor.
 
Sample Input 0
1
4
5
9
 
Sample Output 0
Yes
 
Explanation 0
start = (1, 4), end = (5, 9)
The bot starts at (1, 4) and makes a move of type 1, meaning that it moves to (1 + 4, 1) = (5, 4). Then it makes a move of type 2 from (5, 4) to (5, 5 + 4) = (5, 9).
The bot successfully moved through the following path: (1, 4) → (5, 4) → (5, 9), which means it was possible to reach (5, 9). Thus, our function returns Yes.
 
Sample Input 1
1
2
2
1
 
Sample Output 1
No
 
Explanation 1
start = (1, 2), end = (2, 1)
Our two types of movement both require an increase in x or y, but the input value for y decreases from the start location to the end location. Thus, it is not possible to reach (2, 1) from (1, 2) and we return No.

-}

canReach :: Int -> Int -> Int -> Int -> Bool
canReach x1 y1 x2 y2 | x1 == x2 && y1 == y2 = True
                     | x1 > x2 || y1 > y2 = False
                     | otherwise = canReach (x1+y1) y1 x2 y2 || canReach x1 (x1+y1) x2 y2
