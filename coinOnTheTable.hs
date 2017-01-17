{-
You have a rectangular board consisting of N rows, numbered from 1 to N, and M columns, numbered from 1 to M. The top left is (1,1) and the bottom right is (N,M). Initially, at time = 0, there is a coin on the top-left cell of your board. Each cell of your board contains one of the following characters:
 
U: If a cell cell (i,j) contains the letter 'U', and that cell contains the coin at time t, then at time t+1 the coin will be on cell (i-1,j). If i = 1 then the coin leaves the board at time t+1.

L: If a cell cell (i,j) contains the letter 'L', and that cell contains the coin at time t, then at time t+1 the coin will be on cell (i,j-1). If j = 1 then the coin leaves the board at time t+1.

D: If a cell cell (i,j) contains the letter 'D', and that cell contains the coin at time t, then at time t+1 the coin will be on cell (i+1,j). If i = N then the coin leaves the board at time t+1.

R: If a cell cell (i,j) contains the letter 'U', and that cell contains the coin at time t, then at time t+1 the coin will be on cell (i,j+1). If i = M then the coin leaves the board at time t+1.

*: Exactly one cell on the board contains the '*' character. When the coin reaches that cell, it will stay there permanently.
 
Before you start the timer to move the coin between the cells, you can make operations to change the board. Your goals is to reach the cell marked with  ‘*’ exactly at time K. An operation consists of selecting any cell other than the one containing the ‘*’ and changing its letter to ‘U’, ‘L’, ‘R’ or ‘D’. Your task is to find the minimum number of operations necessary to achieve the goal.
 
Input:
The first line of input contains three integers N, M and K. Each of the following N lines contains M letters, with no spaces, that describe the board.
 
Output:
Print an integer which represents the minimum number of operations required to achieve your goal. If you cannot achieve your goal, print -1 .
 
Constraints
N, M <= 51
K <= 1000
 
Sample input #1:
2 2 3  
RD  
*L
Sample output :
0
 
Sample input #2:
2 2 1  
RD  
*L
 
Sample output #2:
1
 
Explanation :
In the first example, no letters need to be changed. In the second example, changing the letter of cell (1,1) to ‘D’ achieves the desired result.
-}

import Data.Tree
import qualified Data.Vector as V

type Board = V.Vector Char
-- ((x-val,y-val), letter, n changes, time to *
type Cell = ((Int,Int), Char, Int, Int)
type Route = Tree Cell

-- cell access 
thd (_, _, y, _) = y
fth (_, _, _, z) = z

-- extend is the categorical dual to canonical (=<<)
(=>>) :: Tree a -> (Tree a -> b) -> Tree b
(=>>) t f = unfoldTree (\a@(Node _ z) -> (f a, z)) t

-- categorical dual to canonical "join"
duplicate :: Tree a -> Tree (Tree a)
duplicate t = t =>> id

-- identity for coflatMapTree
extract :: Tree a -> a
extract (Node r _) = r


