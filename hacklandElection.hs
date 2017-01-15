import           Data.List
votes = ["Alex","Michael","Harry","Dave","Michael","Victor","Harry","Alex","Mary","Mary"]

inList :: (Eq a) => a -> [a] -> [a]
inList x ll
            | elem x ll = ll
            | otherwise = x : ll

unique :: (Eq a) => [a] -> [a]
unique ll = foldr (inList) [] ll

count :: (Eq a) => a -> [a] -> Int
count x ll = foldr (\i c -> if(i==x) then c+1 else c) 0 ll

histogram :: [String] -> [(String, Int)]
histogram names =
    let uniqueNames = unique names
    in map (\uniqueName -> (uniqueName, count uniqueName names)) uniqueNames

votesHistogram :: [(String, Int)]
votesHistogram = histogram votes

tupleSort :: (String, Int) -> (String, Int) -> Ordering
tupleSort (s1, i1) (s2, i2)
    | i2 > i1 = LT
    | i2 == i1 && s2 > s1 = LT
    | otherwise = GT


orderedHistogram :: [String] -> [(String, Int)]
orderedHistogram names =
    let hist = histogram names
    in sortBy tupleSort hist


orderedVotes = orderedHistogram votes

electionWinner :: [String] -> String
electionWinner vts =
    let ordVts = orderedHistogram vts
    in fst $ last ordVts
