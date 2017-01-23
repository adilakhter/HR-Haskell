{-
You are working on a computer simulation of a mobile robot. The robot moves on an infinite plane, starting from position (0, 0). Its movements are described by a command string consisting of one or more of the following three letters:
G instructs the robot to move forward one step.
L instructs the robot to turn left.
R instructs the robot to turn right.
 
The robot performs the instructions in a command and repeats them for an infinite time. You want to know whether or not there exists some circle whose radius is a positive real number such that the robot always moves within the circle and never leaves it.
 
Complete the doesCircleExist function in the editor below. It has one parameter: an array of strings, commands. The function must return an array of n strings where each element i denotes whether or not performing commandsi on an infinite loop will restrict the robot's movements to a circle. If the instruction restricts the robot's movement to a circle, set index i to "YES"; otherwise, set it to "NO".
 
Input Format
Locked stub code in the editor reads the following input from stdin and passes it to the function:
The first line contains an integer, n, denoting the number of elements in commands.
Each line i of the n subsequent lines (where 0 ≤ i < n) contains a string describing commandsi.
 
Constraints
1 ≤ length of commandsi ≤ 2500
1 ≤ n ≤ 10
Each command consists of G, L, and R only.
 
Output Format
The function must return an array of strings where each element i contains either the string YES or NO denoting whether or not the robot's movements will be restricted to some circle if it performs instruction commandsi on an infinite loop. This is printed to stdout by locked stub code in the editor.
 
Sample Input 0
2
G
L
 
Sample Output 0
NO
YES
 
Explanation 0
We must consider the following n = 2 commands:
For commands0 = "G", the robot will move forward forever without ever turning or being restricted to a circle. Thus, we set index 0 of our return array to "NO".
For commands1 = "L", the robot will just turn 90 degrees left forever without ever moving forward (because there is no "G" instruction). Because the robot is effectively trapped in a circle, we set index 1 of our return array to "YES".
 
Sample Input 1
1
GRGL
 
Sample Output 1
NO
 
Explanation 1
Let's consider the robot's initial orientation to be facing north toward the positive y-axis. The robot performs the following four steps in a loop:
Go forward one step. The robot moves from (0, 0) to (0, 1).
Turn right. The robot turns eastward, facing the positive x-axis.
Go forward one step. The robot moves from (0, 1) to (1, 1).
Turn left. The robot turns northward, facing the positive y-axis again.
The robot then repeats these steps infinitely, following an endless zig-zagging path in the northeastern direction. Because the robot will never turn in such a way that it would be restricted to a circle, we set index 0 of our return array to "NO".
-}

import Control.Monad.State
import Data.Traversable (traverse)
import Data.List

type Location = (Int,Int,Char)

left :: State Location Location
left = do
  (x,y,dir) <- get
  let dir' = case dir of
        'N' -> 'W'
        'E' -> 'N'
        'S' -> 'E'
        'W' -> 'S'
      new = (x,y,dir')
  put new    
  return new

right :: State Location Location
right = do
  (x,y,dir) <- get
  let dir' = case dir of
        'N' -> 'E'
        'E' -> 'S'
        'S' -> 'W'
        'W' -> 'N'
      new = (x,y,dir')
  put new    
  return new
  
forward :: State Location Location
forward = do
  (x,y,dir) <- get
  let (x',y') = case dir of
        'N' -> (x,y+1)
        'E' -> (x+1,y)
        'S' -> (x,y-1)
        'W' -> (x-1,y)
      new = (x',y',dir)
  put new    
  return new
  
parseC :: Char -> State Location Location
parseC 'L' = left
parseC 'R' = right
parseC 'G' = forward

parseS :: String -> State Location [Location]
parseS = traverse parseC

output False = "NO"
output True = "YES"

doesCircleExist commands = let
  commands' = map (\s -> concat $ replicate 4 s) commands
  start = (0,0,'N')
  robots = map parseS commands'
  finishes = map (last . (flip evalState start)) robots
  encircled = map (==start) finishes
  in map output encircled
