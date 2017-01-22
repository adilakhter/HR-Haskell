import Control.Monad
import Text.Regex (splitRegex, mkRegex)
import Data.List

tokenize :: String -> [String]
tokenize = splitRegex (mkRegex "[[:space:],]")

-- "1 2 3" -> [1,2,3]
getInts :: IO [Int]
getInts = getLine >>= (\x -> return $ fmap (\x -> read x :: Int) $ tokenize x)

-- unsafe
submatrix :: Int -> Int -> Int -> Int -> [[Int]] -> [[Int]]
submatrix r1 c1 r2 c2 mat =
  let cdiff = c2-c1
      rdiff = r2-r1
      rows = take (rdiff+1) . drop r1 $ mat
      submat = fmap (\row -> take (cdiff+1) . drop c1 $ row) rows
  in submat

flatSubmatrix :: Int -> Int -> Int -> Int -> [[Int]] -> [Int]
flatSubmatrix r1 c1 r2 c2 mat =
  concat $ (submatrix r1 c1 r2 c2 mat)

average :: Int -> Int -> Int
average a b = div (a + b) 2

median :: [Int] -> Int
median ll 
  | (mod (length ll) 2) == 1 = ll !! div (length ll) 2
  | otherwise = let a = ll !! (div (length ll) 2 - 1)
                    b = ll !! div (length ll) 2
                in (average a b)

queryMedianString :: [[Int]] -> IO Int
queryMedianString mat = do
  tokens <- getInts
  let r1 = tokens !! 0
      c1 = tokens !! 1
      r2 = tokens !! 2
      c2 = tokens !! 3
      med = queryMedianOneIndexed r1 c1 r2 c2 mat
  return med

queryMedianOneIndexed :: Int -> Int -> Int -> Int -> [[Int]] -> Int
queryMedianOneIndexed r1 c1 r2 c2 mat =
  queryMedian (r1-1) (c1-1) (r2-1) (c2-1) mat 

queryMedian :: Int -> Int -> Int -> Int -> [[Int]] -> Int
queryMedian r1 c1 r2 c2 mat =
  let flatSubmat = flatSubmatrix r1 c1 r2 c2 mat :: [Int]
      sortedFlatSubmat = sort flatSubmat :: [Int]
      med = median sortedFlatSubmat :: Int
  in med
  

main :: IO ()
main = do
  n <- readLn :: IO Int
  matrix <- replicateM n getInts :: IO [[Int]]
  m <- readLn :: IO Int
  medians <- replicateM m $ queryMedianString matrix :: IO [Int]
  mapM_ (putStrLn . show)  medians

r1 = 1
c1 = 1
r2 = 2
c2 = 2
mat = [
  [1,3,2,4],
  [8,1,2,9],
  [1,1,2,2],
  [7,5,3,6]
  ]

r12 = 3
c12 = 3
r22 = 4
c22 = 4

mat2 = [
  [1,1,1,1,1],
  [1,1,1,1,1],
  [1,1,1,1,1],
  [1,1,1,1,1],
  [1,1,1,1,1]
  ]
r13 = 3
c13 = 3
r23 = 5
c23 = 5

r14 = 3
c14 = 3
r24 = 4
c24 = 4

submat = submatrix (r1-1) (c1-1) (r2-1) (c2-1) mat

flatSubmat = flatSubmatrix (r1-1) (c1-1) (r2-1) (c2-1) mat

submat2 = submatrix (r12-1) (c12-1) (r22-1) (c22-1) mat

med = queryMedian (r1-1) (c1-1) (r2-1) (c2-1) mat 
med2 = queryMedian (r12-1) (c12-1) (r22-1) (c22-1) mat

med3 = queryMedianOneIndexed r13 c13 r23 c23 mat2
submat3 = submatrix r13 c13 r23 c23 mat2
med4 = queryMedianOneIndexed r14 c14 r24 c24 mat2
submat4 = submatrix r14 c14 r24 c24 mat2
submatFlat4 = flatSubmatrix r14 c14 r24 c24 mat2
