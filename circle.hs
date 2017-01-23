import           Control.Monad
import           Data.List
import           Text.Regex    (mkRegex, splitRegex)

tokenize :: String -> [String]
tokenize = splitRegex (mkRegex "[[:space:],]")

-- "1 2 3" -> [1,2,3]
getInts :: IO [Int]
getInts = getLine >>= (\x -> return $ fmap (\x -> read x :: Int) $ tokenize x)

--           index, radius, cost
type Distance = Int
type Radius = Int
type Cost = Int
type Gear = (Int, Radius, Cost)

getIndex :: Gear -> Int
getIndex (i, _, _) = i

getRadius :: Gear -> Radius
getRadius (_, r, _) = r

getCost :: Gear -> Cost
getCost (_, _, c) = c


meetsDistance :: Distance -> Gear -> Gear -> Bool
meetsDistance d gear1 gear2 = getRadius gear1 + getRadius gear2 >= d

sortByCost :: Gear -> Gear -> Ordering
sortByCost gear1 gear2
    | getCost gear1 < getCost gear2 = LT
    | getCost gear1 == getCost gear2 && getRadius gear1 < getRadius gear2 = LT
    | otherwise = GT

getComplementaryGear :: Distance -> [Gear] -> Gear -> Maybe Int
getComplementaryGear d gears gear =
    let
        qualifiedGears = filter (meetsDistance d gear) gears :: [Gear]
        sortedQualifiedGears = sortBy sortByCost qualifiedGears :: [Gear]
        -- maybeHeadTail = uncons sortedQualifiedGears :: Maybe (Gear, [Gear])
        maybeGear = find (\_ -> True) sortedQualifiedGears :: Maybe Gear
    in fmap getIndex maybeGear :: Maybe Int
    --in fmap (getIndex . fst) maybeHeadTail :: Maybe Int

getComplementaryGears :: Distance -> [Gear] -> [Maybe Int]
getComplementaryGears d gears = fmap (getComplementaryGear d gears) gears

folder :: Maybe Int -> Int
folder (Just i) = i
folder Nothing  = 0

main :: IO ()
main = do
  firstLine <- getInts
  radii <- getInts
  costs <- getInts
  let
      n = firstLine !! 0
      d = firstLine !! 1
      gears = zip3 [1..n] radii costs
      complementaryGears = getComplementaryGears d gears
      answer = fmap folder complementaryGears :: [Int]
  mapM_ (putStr . (\s -> s ++ " ") . show) answer



n = 5
d = 8
radii = [1,3,6,2,5]
costs = [5,6,8,3,4]
gears = zip3 [1..n] radii costs
cmplGears = getComplementaryGears d gears

