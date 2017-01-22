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

Sample input #2:
3 3 4  
RLL
RDD
*LR


-}

import Data.Tree
import Data.List
import Data.Maybe
import Control.Monad (join, replicateM)
import qualified Data.Vector as V
import Text.Regex (splitRegex, mkRegex)



-- Rose tree comonad implementation

-- categorical dual to bind (>>=)
(=>>) :: Tree a -> (Tree a -> b) -> Tree b
(=>>) t f = unfoldTree (\a@(Node _ z) -> (f a, z)) t

-- categorical dual to join
cojoin :: Tree a -> Tree (Tree a)
cojoin t = t =>> id

--categorical dual to return
coreturn :: Tree a -> a
coreturn (Node r _) = r


type Board = V.Vector Char
-- (address, label, n changes, depth)
type Cell = ((Int,Int), Char, Int, Int)
-- Tree Cell [r,d,l,u]
type Routes = Tree Cell




-- board-related functions
getChanges (_, _, y, _) = y
getAddress (w, _, _, _) = w
getDepth (_, _, _, z) = z
getLabel (_, x, _, _) = x
 
getIndex :: Cell -> Int
getIndex cell = case cell of
  (_,'R',_,_) -> 0
  (_,'D',_,_) -> 1
  (_,'L',_,_) -> 2
  (_,'U',_,_) -> 3
  (_,'*',_,_) -> -1
  (_,'O',_,_) -> -1
  
  
-- solver
index2Label i = case i of
  0 -> 'R'
  1 -> 'D'
  2 -> 'L'
  3 -> 'U'
  
update :: Routes -> Cell
update (Node cell []) = cell
update (Node cell routes) = let
  routes' = map update routes
  changes = map getChanges routes'
  d = getDepth cell
  a = getAddress cell
  i = getIndex cell
  i' = fromJust $ findIndex (==minimum changes) changes
  oldCell = routes' !! i
  minCell = routes' !! i'
  cell' = (a,index2Label i,getChanges oldCell,d)
  cell'' = (a,index2Label i',getChanges minCell + 1,d)
  output = if (getChanges minCell) < (getChanges oldCell) then cell'' else cell'
  in if i==(-1) then cell else output

hasStar :: Routes -> Bool
hasStar (Node cell routes) = case cell of
  (_,'*',_,0) -> True
  (_,_  ,_,0) -> False
  otherwise -> or $ map hasStar routes
  
--IO
tokenize :: String -> [String]
tokenize = splitRegex (mkRegex "[[:space:],]")

getInts :: IO [Int]
getInts = getLine >>= (\x -> return $ fmap (\x -> read x :: Int) $ tokenize x)

getBoard :: Int -> IO Board
getBoard n = fmap (V.fromList . join) $ replicateM n getLine


main :: IO ()
main = do
  [n,m,k] <- getInts
  board <- getBoard n
  let
    -- build tree of possible routes
    outOfBounds :: (Int,Int) -> Bool
    outOfBounds (i,j) = let
      vert = (i>=n) || (i<0)
      horz = (j>=m) || (j<0)
      in vert || horz
  
    getLabel' :: (Int,Int) -> Char
    getLabel' (i,j) = let
      index = i*m + j
      in if outOfBounds (i,j) then 'O' else board V.! index

    getCell :: Int -> (Int,Int) -> Cell
    getCell k address = let
      c = getLabel' address
      cell  = (address, c, 0, k)
      cell' = (address, c, 199, k) --fix?
      out = outOfBounds address
      in case (k,c,out) of
        (0,'*',_) -> cell
        (0,_  ,_) -> cell'
        (k,_  ,False) -> cell
        (k,_  ,True ) -> cell'

    iter :: Int -> Cell -> Routes    
    iter (-1) cell = Node cell []
    iter k cell = let
      neighbors = getNeighbors k cell
      in Node cell $ map (iter (k-1)) neighbors

    getNeighbors :: Int -> Cell -> [Cell]
    getNeighbors k cell = let
      (i,j) = getAddress cell
      r = getCell k (i,j+1)
      d = getCell k (i+1,j)
      l = getCell k (i,j-1)
      u = getCell k (i-1,j)
      in [r,d,l,u]

    fillTree :: Board -> Int -> Routes
    fillTree board k = let
      start = getCell k (0,0)
      in if (k==0) then Node start [] else iter (k-1) start

    routes = fillTree board k
    possible = hasStar routes
    changes = getChanges $ update routes
    output = if possible then changes else -1
    
  print output




{-
final s = let
  possible = coreturn $ routes =>> hasStar

n = 3
m = 3
k = 4

board :: Board
board = V.fromList "RLLRDD*LR"
-}
