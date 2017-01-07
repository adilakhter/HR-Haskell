{-
Implement a simple stack that accepts the following commands and performs the operations associated with them:
push k: Push integer k onto the top of the stack.
pop: Pop the top element from the stack. It is guaranteed that this will not be called on an empty stack.
inc e k: Add k to each of the bottom e elements of the stack.
 
Complete the superStack function in the editor below. It has one parameter: an array of n strings, operations. The function must create an empty stack and perform each of the n operations in order and, after performing each operation, print the value of the stack's top element on a new line; if the stack is empty, print EMPTY instead.
 
Input Format
Locked stub code in the editor reads the following input from stdin and passes it to the function:
The first line contains an integer, n, denoting the number of operations to perform on the stack.
Each line i of the n subsequent lines contains a space-separated string describing operationsi.
 
Constraints
1 ≤ n ≤ 2 × 105
-109 ≤ k ≤ 109
1 ≤ e ≤ |S|, where |S| is the size of the stack at the time of the operation.
It is guaranteed that pop is never called on an empty stack.
 
Output Format
After performing each operation, print the value of the stack's top element on a new line; if the stack is empty, print EMPTY instead.
 
Sample Input 0
12
push 4
pop
push 3
push 5
push 2
inc 3 1
pop
push 1
inc 2 2
push 4
pop
pop
 
Sample Output 0
4
EMPTY
3
5
2
3
6
1
1
4
1
8
-}

import Control.Monad.State
import Data.List (intercalate)

type Stack = [Int]


pop :: State Stack String
pop = do
  stack <- get
  case stack of
    [x] -> state $ \[x] -> ("EMPTY", [])
    _ -> state $ \(x:xs) -> (show $ head xs,xs)

push :: Int -> State Stack String
push x = state $ \xs -> (show x,x:xs)

inc :: [Int] -> State Stack String
inc [e,k] = do
  stack <- get
  let n = length stack - e
  let top = take n stack
  let bottom = drop n stack
  let out = top ++ (map (+k) bottom)
  put out
  return $ show $ head out

parseArg :: String -> State Stack String
parseArg "pop" = pop
parseArg ('p':'u':'s':'h':' ':xs) = push (read xs :: Int)
parseArg ('i':'n':'c':' ':xs) = inc (chain reads xs :: [Int])

chain :: (s -> [(a, s)]) -> s -> [a]
chain f s = case f s of
  [] -> []
  [(a, newS)] -> a : chain f newS
  xs -> map fst xs ++ chain f (last $ map snd xs)
  
instructions = ["push 4", "pop", "push 3", "push 5", "push 2", "inc 3 1", "pop", "push 1", "inc 2 2", "push 4", "pop", "pop"]

superStack operations = do
  let s = forM operations parseArg
  mapM_ putStrLn (evalState s [])

main = superStack instructions

