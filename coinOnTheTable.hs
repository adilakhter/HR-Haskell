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
import Data.Maybe
import Prelude hiding (fst,snd)
import Control.Monad (join)
import qualified Data.Vector as V

--data Tree a = Tree a [Tree a]
    
type Board = V.Vector Char
-- ((x-val,y-val), letter, n changes, depth
type Cell = ((Int,Int), Char, Int, Int)
-- Tree Cell [u, d, l, r]
type Routes = Tree Cell

getNeighbors :: Cell -> [Cell]
getNeighbors cell = let
  (i,j) = fst cell
  r = getCell (i,j+1)
  d = getCell (i+1,j)
  l = getCell (i,j-1)
  u = getCell (i-1,j)
  in catMaybes [r,d,l,u]

iter :: Int -> Cell -> Routes
iter 0 cell = Node cell []
iter k cell = let
  neighbors = getNeighbors cell
  in Node cell $ map (iter (k-1)) neighbors

fillTree :: Board -> Int -> Routes
fillTree board k = let
  start = fromJust $ getCell (0,0)
  in if (k==0) then Node start [] else iter k start

-- primitives and Rose tree comonad implementation
-- cell access
fst (w, _, _, _) = w
snd (_, x, _, _) = x
thd (_, _, y, _) = y
fth (_, _, _, z) = z

-- extend is the categorical dual to bind (>>=)
(=>>) :: Tree a -> (Tree a -> b) -> Tree b
(=>>) t f = unfoldTree (\a@(Node _ z) -> (f a, z)) t

-- duplicate is the categorical dual to join
duplicate :: Tree a -> Tree (Tree a)
duplicate t = t =>> id

-- identity for extend
extract :: Tree a -> a
extract (Node r _) = r

-- board-related functions

width = 2
n = 2
m = 2

board :: Board
board = V.fromList ['R','D','*','L']

readBoard :: [[Char]] -> Board
readBoard l = V.fromList $ join l


outOfBounds :: Cell -> Bool
outOfBounds cell = let
  (i,j) = fst cell
  vert = (i>=n) || (i<0)
  horz = (j>=m) || (j<0)
  in vert || horz

-- board is stored in row-major order, i and j are zero-indexed
getLabel :: (Int,Int) -> Char
getLabel (i,j) = let
  index = i*width + j
  in board V.! index

getCell :: (Int,Int) -> Maybe Cell
getCell tup = let
  c = getLabel tup
  cell = (tup, c, 0, 0)
  in if outOfBounds cell then Nothing else Just cell
  
goal :: Routes
goal = Node ((1,0), '*', 3, 0) []

goal1 :: Routes
goal1 = Node ((1,1), 'L', 2, 0)
             [ Node ((0,1), 'D', 3, 0) [], --U
               Node ((1,1), 'O', 3, 0) [], --D
               Node ((1,1), '*', 3, 0) [], --L
               Node ((1,1), 'O', 3, 0) []  --R
             ]
