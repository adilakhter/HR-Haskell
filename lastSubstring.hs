import           Data.List

-- http://stackoverflow.com/a/24599918/1007926
gather l n = filter ((== n) . length) $ map (take n) $ tails l

compute :: String -> String
compute word =
    let wordLength = length word
        lengths = [1..wordLength]
        gatherings = map (gather word) lengths
        allGatherings = concat gatherings
        sorted = sort allGatherings
    in last sorted
