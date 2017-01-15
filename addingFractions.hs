{-
Your task is to find the sum of two fractions, expressed in the form X/Y and U/V, where X, Y, U, V are four integers. Compute their sum and reduce it to its lowest indivisible state: A/B.
 
For example,
2/6+2/6 equals 4/6, which should be reduced to 2/3.
7/10+13/10 equals 20/10 which should be reduced to 2/1.
..and so on
 
Complete the fractionSum function in your editor. It has 1 parameter:
A string array str.
 
It must return a string array, where ith element is result for the ith test case.
 
Note: Each output should contain a fraction A/B corresponding to the sum of the fractions, in an irreducible state.  
 
Input Format
The locked stub code in your editor reads the following input from stdin and passes it to your function:
The first line contains an integer, N which is the number of fractions which will follow.
This is followed by N lines, each containing an expression representing the sum of two fractions: provided in the form X/Y+U/V
 
Constraints
1 <= X, Y, U, V <= 2000
1 <= N <= 500
 
Output Format
Your function must return a string array, where ith element is result for the ith test case. This is printed to stdout by the locked stub code in your editor.
 
Sample Input 1
5
722/148+360/176
978/1212+183/183
358/472+301/417
780/309+684/988
258/840+854/686
 
Sample Output 1
2818/407
365/202
145679/98412
4307/1339
1521/980
 
Sample Input 1
10
241/26+612/398
278/348+246/157
156/53+952/760
263/418+560/304
540/28+44/295
636/78+354/868
68/415+1041/807
147/269+224/844
759/120+370/72
654/162+476/307
 
Sample Output 1
55915/5174
64627/27318
21127/5035
1033/418
40133/2065
48305/5642
162297/111635
46081/56759
4127/360
46315/8289
 -}

import Data.Ratio
import Text.Regex (subRegex, mkRegex)
import Text.Regex.Posix

parse :: String -> Rational
parse str = let
  (_, _, _, [a1, a2, b1, b2]) = str =~ "([0-9]*)/([0-9]*)\\+([0-9]*)/([0-9]*)" :: (String,String,String,[String])
  fracA = (read a1 :: Integer) % (read a2 :: Integer)
  fracB = (read b1 :: Integer) % (read b2 :: Integer)
  in fracA + fracB

output :: Rational -> String
output r = subRegex (mkRegex " % ") (show r) "/"

fractionSum str =map (output . parse) str 
