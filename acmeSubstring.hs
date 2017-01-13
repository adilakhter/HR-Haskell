{-
Inhabitants of the planet ACME have uncovered a secret chamber inside one of their ancient pyramids, containing a scroll with a continuous string A consisting of lowercase ASCII characters. A is followed by a smaller query string X. They are telepathically directed to find the position of the first occurrence of X within A, and enter it into the pyramid mainframe. The smaller query string X may also contain a wildcard character (*) which matches any lowercase ASCII character.

Task
You are given a function firstOccurrence which takes in two strings A and X as parameters.  Can you complete the function to find the position of X and return the starting position? The code ot handle input and output is already provided in the development environment.

Constraints
The notation |s| refers to the length of a string s.
1 ≤ |A| ≤ 20,000,000
1 ≤ |X| ≤ 1,000

Output Format
Complete the function and return the (0-origin) position of the first occurrence of the string X in A. If X is not in A, then return -1.

Sample Input
thisisatest
this

Sample Output
0

Explanation
The string 'this' (X) first occurs at position 0 in 'thisisatest' (A).

replace :: Char -> Char
replace '\160' = 'X'
replace c      = c

test = map replace "a\160b" == "aXb"
-}

import Text.Regex.Posix
import Text.Regex.Base
import Text.Regex
import qualified Data.ByteString.Char8 as BS

{-
asterisk = mkRegex "*"

firstOccurrence :: BS.ByteString -> BS.ByteString -> Int
firstOccurrence a  b =
  let b' = BS.unpack b
      b'' = BS.pack $ subRegex (mkRegex "*") b' "[:lower:]"
  in fst $ (a =~ b'' :: (Int , Int))
-}

firstOccurrence :: BS.ByteString -> BS.ByteString -> Int
firstOccurrence a  b = fst $ (a =~ b :: (Int , Int))

main = do
    _a <- BS.getLine
    _b <- BS.getLine
    let res = firstOccurrence _a _b 
    print res
