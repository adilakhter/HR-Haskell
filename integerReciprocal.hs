
import Data.List

zipWithIndex :: [a] -> [(Int, a)]
zipWithIndex l = zip [1..] l

folder :: Int -> (Int, a) -> [[a]] -> [[a]]
folder n (i, x) (l:ll)
  | mod i n == 0 = [x]:(l:ll)
  | otherwise = (x:l):ll

  
groupN :: Int -> [a] -> [[a]]
groupN n l =
  foldr (folder n) [[]] $ zipWithIndex l

coprime :: Int -> Int -> Bool
coprime x y = gcd x y == 1

--cycle :: 

reciprocal :: Int -> IO ()
reciprocal n
  | coprime n 10 == False =
    let
      frac = 1 / (fromIntegral n)
      fracString = show frac
    in putStrLn fracString
  | otherwise =
    let
      frac = 1 / (fromIntegral n)
      

x = [1..20]
xGrouped = groupN 3 x
  
