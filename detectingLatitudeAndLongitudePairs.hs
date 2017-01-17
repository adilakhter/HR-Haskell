{-
Complete the function isValid, that has one parameter, array, coordinates, of n strings. The function should print Valid if ith coordinate in the array is a valid representation of latitude and longitude, otherwise, it should print Invalid.
 
A valid representation will adhere to following points
The latitude and longitude, if present, will always appear in the form of (X, Y) where X and Y are decimal numbers, and the '(', ')' and ',' are exactly copied.
For a valid (latitude, longitude) pair:
-90 ≤ X ≤ +90
-180 ≤ Y ≤ 180
They will not contain any symbols for degrees or radians or N/S/E/W.
There may or may not be a +/- sign preceding X or Y.
There will be a comma followed by a space between X and Y as shown here (X, Y).
There will be no space
between X and the preceding left-bracket, or
between Y and the following right-bracket, or
between X and the following comma, or
before the initial '('.
There will be no leading zeros before X or Y, and no decimal point that is not followed by non-zero digits.
 
Input Format
The first line contains an integer n, the number of tests.
This is followed by n lines of text, each of which contains a pair of coordinates which may indicate the latitude and longitude of a place.

Constraints
1 ≤ n ≤ 100
 
Output Format
One line of text per test case, with the string Valid if  X and Y are a valid (latitude, longitude) pair, otherwise, Invalid.

Sample Input
12
(75, 180)
(+90.0, -147.45)
(77.11112223331, 149.99999999)
(+90, +180)
(90, 180)
(-90.00000, -180.0000)
(75, 280)
(+190.0, -147.45)
(77.11112223331, 249.99999999)
(+90, +180.2)
(90., 180.)
(-090.00000, -180.0000)
 
Sample Output
Valid
Valid
Valid
Valid
Valid
Valid
Invalid
Invalid
Invalid
Invalid
Invalid
Invalid
 
Explanation
The first six pairs are valid because X and Y satisfy the criteria for numerical values and formats.
The next six pairs are all invalid because  Four has numbers outside the numerical ranges for X and Y.
(90., 180.) is invalid because of an extra decimal point (.) after 90 and 180.
(-090.0000, -180.0000) is invalid because of the leading zero before 90 and zeros after the decimal point (.).


-}

import Control.Monad
import Text.Parsec
import Control.Applicative hiding ((<|>))

type Parser = Parsec String ()

number = many1 digit

plus = char '+' *> number

minus = (:) <$> char '-' <*> number

integer = plus <|> minus <|> number

tuple :: Parser (Float, Float)
tuple = do
  char '('
  a <- float
  string ", "
  b <- float
  char ')'
  return (a,b)

float = fmap rd $ (++) <$> integer <*> decimal
  where rd      = read :: String -> Float
        decimal = option "" $ (:) <$> char '.' <*> number

valid (lat, long) = let
  a = -90.0 <= lat && lat <= 90.0
  b = -180.0 <= long && long <= 180.0
  in if a && b then "Valid" else "Invalid"

checkTuple :: String -> String
checkTuple t = either (const "Invalid") valid $ parse tuple "" t

isValid :: [String] -> IO ()
isValid coordinates = let
  outputs = map checkTuple coordinates
  in mapM_ putStrLn $ outputs

