
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

-- repetitionN :: Int -> String -> Bool
-- repetitionN n s =
--   let firstN = take n s
--       tail = drop n s
--   in foldr

-- reciprocal :: Int -> IO ()
-- reciprocal n = let
--   frac = 1 / (fromIntegral n)
--   fracString = show frac
--   (left, right) = splitAt 3 fracString
--   short = left ++ " " ++ take 1 right
--   in putStrLn $ short


x = [1..20]
xGrouped = groupN 3 x
  
