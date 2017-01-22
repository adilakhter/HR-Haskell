{-
Problem Statement
Gridland is a 2D world divided into many cells; each cell is described as a coordinate (x,y) where x≥0 and y≥0. From some cell (x,y), an inhabitant can move to an adjacent cell (i.e.: (x+1,y), (x-1,y), (x,y+1), or (x,y-1)). Bob is standing in cell (0,0) and wants to move to cell (x,y) in a minimum number of moves. There are many possible minimum paths, some of which are blocked by dangerous dragons; however, Bob knows that if he orders his minimum paths lexicographically and takes the kth minimum path, he can avoid the dragons!
 
All possible minimum paths can be described as as sequence of horizontal (H) and vertical (V) moves. For example, one possible path from (0,0) to (2,2) is HVHV, meaning he first moves horizontally, then vertically, then horizontally again, and then vertically again. Another possible path is HVVH, but HVHV is lexicographically smaller than HVVH.
 
Given a string comprised of three space-separated integers (in the form "x y k"), find the lexicographically kth path using a minimum number of moves to travel from (0,0) to (x,y). Complete the function gridLand so that it returns a string array of kth minimum paths where each element i in the return array is the kth minimum path for inp[i].
 
Note: k is in the inclusive range of 0 to P-1, where P is the number of possible minimum paths. In the example above, HHVV is the lexicograpically 0th minimum path.
 
Input Parameters
Each string element in inp is comprised of three space-separated integers; the first is x (the destination x coordinate), the second is y (the destination y coordinate), and the third is k (recall that the kth lexicographically minimum path is the one not blocked by dragons).
 
Constraints
1 ≤ |inp| ≤ 100000 
1 ≤ x ≤ 10 
1 ≤ y ≤ 10 
0 ≤ k < number of paths
 
Output Parameters
Return an array of strings where the ith element is the answer for the ith element (test case) in string array inp.
 
Sample Input
The inp array passed as an argument to gridLand has size 2 and contains the following data:
inp[0] = "2 2 2"
inp[1] = "2 2 3"
 
Sample Output
HVVH
VHHV
 
Explanation
For test case inp[0], the 6 minimum paths from (0,0) to (2,2) in lexicographically increasing order are:
 
0. HHVV
1. HVHV
2. HVVH
3. VHHV
4. VHVH
5. VVHH
 
Because k=2, we print the kth lexicographical minimum path, which is HVVH.
Haskell


-}

import Data.Tree
import Data.List
import Data.Maybe
import Control.Monad (join, replicateM)

import Text.Regex (splitRegex, mkRegex)

type Address = (Int, Int)
type Move = Char
type Depth = Int
type Cell = (Address, Move, Depth)
type Routes = Tree Cell

getAddress (a, _, _) = a
getMove (_, m, _) = m
getDepth (_, _, d) = d

getChildren :: Depth -> Cell -> [Cell]
getChildren k parent = let
  (i,j) = getAddress parent
  r = ((i,j+1),'H',k)
  d = ((i+1,j),'V',k)
  in [r,d]

fillTree :: Int -> Routes
fillTree k = let
  start = ((0,0),'O',k)
  
  iter :: Depth -> Cell -> Routes    
  iter (-1) cell = Node cell []
  iter k cell = let
    children = getChildren k cell
    in Node cell $ map (iter (k-1)) children
    
  in if (k==0) then Node start [] else iter (k-1) start

onPath :: Address -> Routes -> Bool
onPath goal (Node cell routes) = let
  p = goal == (getAddress cell)  
  check = onPath goal
  in p || (or $ map check routes)
  
prune :: Address -> Routes -> Routes
prune goal (Node cell routes) = let
  p = goal == (getAddress cell)
  check = prune goal
  routes' = filter (onPath goal) routes
  in if p then Node cell [] else Node cell $ map check routes'
  
getRoutes :: Routes -> [String]
getRoutes (Node cell []) = [(getMove cell):[]]
getRoutes (Node cell routes) = let
  routes' = routes >>= getRoutes
  in map ((getMove cell):) routes'

solve :: [Int] -> String
solve [n,m,k] = let
  goal = (n,m)
  routes = prune goal $ fillTree (n+m)
  output = sort $ map tail $ getRoutes routes
  in output !! k
  
--IO

tokenize :: String -> [String]
tokenize = splitRegex (mkRegex "[[:space:],]")

parse :: String -> [Int]
parse s = let
  l = tokenize s
  in map (\x -> read x :: Int) l
  
gridLand inp = let
  input = map parse inp
  in map solve input
  
