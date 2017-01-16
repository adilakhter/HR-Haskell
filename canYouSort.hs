import           Data.List
import           Data.Ord

foo = [8,5,5,5,5,1,1,1,4,4]

inList :: (Eq a) => a -> [a] -> [a]
inList x ll
            | elem x ll = ll
            | otherwise = x : ll

unique :: (Eq a) => [a] -> [a]
unique ll = foldr (inList) [] ll

count :: (Eq a) => a -> [a] -> Int
count x ll = foldr (\i c -> if(i==x) then c+1 else c) 0 ll

histogram :: [Int] -> [(Int, Int)]
histogram numbers =
    let uniqueNumbers = unique numbers
    in map (\uniqueNumber -> (count uniqueNumber numbers, uniqueNumber)) numbers

sortedHistogram :: [Int] -> [(Int, Int)]
sortedHistogram numbers =
    let hist = histogram numbers
        sortedHist = sort hist
    in sortedHist

sortedFoo = sortedHistogram foo

customSort :: [Int] -> IO ()
customSort [] = putStrLn ""
customSort arr =
    let out = map snd $ sortedHistogram arr
    in mapM_ print out


example = customSort foo
