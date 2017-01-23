{-
The format of a date needs to be changed in the following manner: 1st Mar 1984 ⇒ 1984-03-01
 
You are provided a list of dates, in [Day Month Year] format shown below. All three segments are separated by spaces.
  1st Mar 1984
  2nd Feb 2013
  4th Apr 1840
The 'Day' may equal any one of the following values: 
1st, 2nd, 3rd, 4th, 5th, 6th, 7th, 8th, 9th ...... 29th, 30th, 31st
 
The 'Month' may equal any one of the following values:
Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec  
 
The 'Year' will always be represented by 4 digits and may equal anything between 1900 and 2100, both inclusive.

Your task is to format the given list of dates into the following format: YYYY-MM-DD
Where:
      YYYY represents the year in 4 digits
      MM   represents the month in 2 digits
      DD   represents the day in 2 digits
 
You have to complete the function reformatDate given in the editor which takes a string array dates as a parameter representing the dates for each test case.

Input Format:
The locked code stub reads the following input from stdin:
The first line, will contain an integer N, which is the number of dates in the provided list.
This will be followed by N dates, each on a separate line, in the format described above.

Constraints:
'Day', 'Month', 'Year' will be confined to the range of values stated above.
All dates provided, will be valid: error handling is not required.
1 ≤ N ≤ 104

Output Format:
Your function must return a string array representing the output for each test case.
 
Sample Input
10
20th Oct 2052
6th Jun 1933
26th May 1960
20th Sep 1958
16th Mar 2068
25th May 1912
16th Dec 2018
26th Dec 2061
4th Nov 2030
28th Jul 1963
 
Sample Output
2052-10-20
1933-06-06
1960-05-26
1958-09-20
2068-03-16
1912-05-25
2018-12-16
2061-12-26
2030-11-04
1963-07-28
 
Explanation
T = 10
Each date has been converted to the format given in problem statement.
-}


import Control.Monad
import Text.Parsec
import Data.List
import Control.Applicative hiding ((<|>),many)


type Parser = Parsec String ()

encode :: (String,String) -> Parser String
encode (mon,n) = do
  try $ string mon
  return n

abbrevs = ["Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"]
nums = ["01","02","03","04","05","06","07","08","09","10","11","12"] 

months = let
  m = map encode $ zip abbrevs nums
  combined = foldl' (<|>) (head m) (tail m)
  in combined
  
date :: Parser (String,String,String)
date = do
  day <- many1 digit
  let day' = if 1 == length day then "0"++day else day
  many1 $ oneOf "fstnrdh"
  char ' '
  month <- months
  char ' '
  year <- many1 digit
  eof
  return (year,month,day')

parseLine line = let
  output = parse date "" line
  agg (year,month,day) = year ++ "-" ++ month ++ "-" ++ day
  in either (const "") agg output
  
reformatDate dates = map parseLine dates
