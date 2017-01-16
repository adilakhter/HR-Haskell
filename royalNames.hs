{-
An ordinal number is a word representing rank or sequential order. The naming convention for royal names is to follow a first name with an ordinal number, which is essentially a Roman numeral used to indicate the birth order of two people having the same name. The Roman numerals from 1 to 50 are defined as follows:
The respective numerals corresponding to numbers 1 through 10 are I, II, III, IV, V, VI, VII, VIII, IX, and X.
The respective numerals corresponding to the numbers 20, 30, 40, and 50 are XX, XXX, XL, and L.
The numeral for any other two-digit number < 50 is constructed by concatenating the numeral(s) for its multiples of ten with the numeral(s) for its values < 10. For example, 47 is 40 + 7 = "XL" + "VII" = "XLVII".
 
Complete the getSortedList function in your editor. It has 1 parameter: an array of royal name strings, names. Each royal name string consists of a first name, followed by a single space, followed by a Roman numeral. Your function must sort the names lexicographically (alphabetically) by first name; if two or more first names are the same, it must sort those duplicate first names by ascending ordinal number. It must then return the sorted array of royal name strings.
 
Input Format
The locked stub code in your editor reads the following input from stdin and passes it to your function:
The first line contains an integer, n (the number of elements in names). Each line i of the n subsequent lines contains a string describing royal name i (where 0 ≤ i < n) in the names array.
 
Constraints
1 ≤ n ≤ 50
Each names[i] (where 0 ≤ i < n) is a single string composed of 2 space-separated values: firstName and ordinal, respectively. Here, firstName is the first name and ordinal is a valid Roman numeral representing a number between 1 and 50, inclusive.
1 ≤ |firstName| ≤ 20
Each firstName starts with an uppercase letter (A − Z) and its remaining characters are lowercase letters (a − z).
Each ordinal is a Roman numeral containing the capital letters I, V, X, and/or L.
The elements in names are pairwise distinct.
 
Output Format
Your function must return a sorted array of royal name strings. This is printed to stdout by the locked stub code in your editor.
 
Sample Input 0
2
Louis IX
Louis VIII
 
Sample Output 0
Louis VIII
Louis IX
 
Sample Input 1
2
Philippe I
Philip II
 
Sample Output 1
Philip II
Philippe I
 
Explanation
Recall that names take precedence over ordinal numbers. This means that the royal names must be sorted lexicographically by name, and then each subset of non-unique names must by sorted by ordinal number.
 
Sample Case 0:
Because both names are the same, we only need to sort by ordinal here. The ordinal number VIII (8) comes before the ordinal number IX (9), so our sorted array must order Louis VIII before Louis IX.
 
Sample Case 1:
When sorted lexicographically, Philip comes before Philippe. Because both names are unique, we do not need to further sort by ordinal. Thus, our sorted array orders Philip II before Philippe I.

  

  
-}


import Control.Exception
import Data.Char 
import Data.List
import Data.Maybe (fromJust, fromMaybe)
import Text.Regex (splitRegex, mkRegex)


tokenize :: String -> [String]
tokenize = splitRegex (mkRegex " ")

parse :: [String] -> (String, Int)
parse [a,b] = (a, fromRoman b)

output (a,b) = a ++ " " ++ (toRoman b)

getSortedList names = let
  names' = map (parse . tokenize) names
  in map output $ sort names'

  

--https://www.cs.york.ac.uk/ftpdir/pub/haskell/contrib/Roman.hs
numerals = [ ('I',   1), ('V',   5), ('X',  10), ('L',  50),
             ('C', 100), ('D', 500), ('M',1000) ]

-- For each numeral, there is a single permitted prefix digit for subtraction.
subnums  = [ ('V','I'),  ('X','I'),  ('L','X'),
             ('C','X'),  ('D','C'),  ('M','C') ]

-- Traverse the numeral list with an accumulator consisting of the
-- string built so far (in reverse order) and the remaining value to be
-- converted.
toRoman   :: Int -> String
toRoman n  = (reverse . snd) (foldr toNumeral (n,"") numerals)

-- Each numeral could potentially appear many times (case 1), and we must
-- also handle (case 2) where a numeral *nearly* fits so we use a subtractive
-- prefix.
toNumeral st@(rdigit, base) (n,s)
  | n >= base    = toNumeral st (n-base, rdigit:s)
  | n+k >= base  = (n-base+k, rdigit:tdigit:s)
  | otherwise    = (n,s)
  where tdigit = fromMaybe '\0' (lookup rdigit subnums)
        k      = fromMaybe  0   (lookup tdigit numerals)


-- The inverse is pretty straightforward by comparison.  First, divide
-- up the string into chunks of identical letters, and add those together
-- (maxmunch).  Then accumulate these from the right - an intermediate
-- letter-sum which is less than the value already accumulated means it
-- must be a prefix subtraction (fromNumeral) rather than an addition.
fromRoman :: String -> Int
fromRoman = foldr fromNumeral 0 . maxmunch . map toUpper

fromNumeral x y
  | x < y  = y-x
  | x > y  = y+x
  
maxmunch "" = []
maxmunch string@(x:_) =
  let (these,those) = span (x==) string
  in fromJust (lookup x numerals) * length these : maxmunch those

