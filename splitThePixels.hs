{-
You are given a sequence of 24 bits that represent a pixel. The leftmost 8 bits give the red component of the pixel, the middle 8 bits give the green component and the rightmost 8 bits give the blue component. The red, green and blue components of the pixel can take values between 0 (binary 00000000) to 255 (binary 11111111) each.
 
You need to identify whether the color of this pixel is geometrically closest to pure red, green, blue, white or black. The geometric distance between two colors with RGB components (r1, g1, b1) and (r2, g2, b2) is given by
d =( (r1 - r2)2 + (g1 - g2)2 + (b1 - b2)2))1/2
 
The RGB value of pure black is (0,0,0), pure white is (255,255,255), pure red is (255,0,0), pure green is (0,255,0), and pure blue is (0, 0, 255).
Your task is to identify which of these five colors is closest to the color of pixel P.  
 
For example, if pixel P is represented by 000000001111111100000110, then
The red component = 00000000 (in binary) = 0 (in base 10)  
The green Component = 11111111 (in binary) = 255 (in base 10)  
The blue Component = 00000110 (in binary) = 6 (in base 10)  
So, the RGB Value of P is (0, 255, 6)
Euclidean Distance of P from pure black (RGB = (0,0,0)):  d = ( (0 - 0)2 + (255 - 0)2 + (6 - 0)2))1/2  = 650611/2
Euclidean Distance of P from pure white (RGB = (255, 255, 255)): d =  ( (0 - 255)2 + (255 - 255)2 + (6 - 255)2))1/2  = 1270261/2
Euclidean Distance of P from pure red (RGB = (255, 0, 0)): d =  ( (0 - 255)2 + (255 - 0)2 + (6 - 0)2))1/2  = 1300861/2
Euclidean Distance of P from pure green (RGB = (0, 255, 0)): d =  ( (0 - 0)2 + (255 - 255)2 + (6 - 0)2))1/2  = 361/2
Euclidean Distance of P from pure blue (RGB = (0, 0, 255)): d =  ( (0 - 0)2 + (255 - 0)2 + (6 - 255)2))1/2  = 1270261/2

From the above, we see, that P is closest to pure green.
If P is equidistant from two or more colors, you should output "Ambiguous" (without quotation marks).
 
Input Format
The first line contains an integer N, which is the number of input pixels to follow.
This is followed by N lines, each containing exactly one sequence of 24 bits (i.e, a sequence of ones and zeroes.)
 
Input Constraints
1 ≤ N ≤ 100
 
Output Format
N lines of text. The ith line in the output should correspond to the ith pixel.
Each line should contain exactly one of the following, depending on which of the named pure colors the pixel is closest to:
Red
Green
Blue
Black
White
Ambiguous
Please ensure the answers are all capitalized exactly as above.
 
Sample Input
5
101111010110011011100100
110000010101011111101111
100110101100111111101101
010111011010010110000011
000000001111111111111111

Sample Output
White
White
White
Green
Ambiguous

parseC :: Parser Color
parseC = do
  r <- binary
  g <- binary
  b <- binary
  return $ Color r g b

-}



import Data.Char (digitToInt)
import Data.List (foldl', nub, findIndices)
import Control.Monad

type Color = [Int]

black :: Color
black = [0,0,0]

white :: Color
white = [255,255,255]

red :: Color
red = [255,0,0]

green :: Color
green = [0,255,0]

blue :: Color
blue = [0,0,255]

splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery n xs = as : splitEvery n bs 
  where (as,bs) = splitAt n xs

toDec :: String -> Int
toDec = foldl' (\acc x -> acc * 2 + digitToInt x) 0

toPixel :: String -> Color
toPixel s = map toDec $ splitEvery 8 s

distSQ :: Color -> Color -> Int
distSQ l1 l2 = foldl' (+) 0 $ map (\(x,y) -> (x-y)^2) $ zip l1 l2

dists :: Color -> [Int]
dists pixel = map (distSQ pixel) [black, white, red, green, blue]
        
getCol :: [Int] -> String
getCol list = let
  minInd list = snd . minimum $ zip list [0..4]
  in case minInd list of
    0 -> "Black"
    1 -> "White"
    2 -> "Red"
    3 -> "Green"
    4 -> "Blue"

isAmbig d = let
  m = minimum d
  l = findIndices (==m) d
  in length l > 1
  
label :: Color -> String
label pixel = let
  d = dists pixel
  in if isAmbig d then "Ambiguous" else getCol d

getClosest :: String -> String
getClosest s = let
  p = toPixel s
  in label p
  
closestColor l = map getClosest l
