{-
Archaeologists on planet Acme just uncovered an ancient temple! Above the entrance, there is an inscription written in lowercase English letters, where each letter is carved into its own stone block. There is a lever next to it that causes a single left-circular rotation of the inscription each time it's pulled. For example, if the inscription says bca, pulling the lever once changes the inscription to cab and pulling it a second time changes the inscription to abc. The door will unlock once the inscription is rotated into a state where it's as lexicographically small as is possible.
 
Complete the function in the editor below. It has one parameter: a string, inscription, describing the inscription over the temple's entrance. The function must return an integer denoting the minimum number of left-circular rotations needed to make inscription as lexicographically small as is possible.
 
Input Format
Locked stub code in the editor reads a string denoting inscription from stdin and passes it to the function.
 
Constraints
The inscription consists of lowercase English alphabetic letters only.
1 ≤ length of inscription ≤ 106
 
Output Format
Return an integer denoting the minimum number of left-circular rotations needed to make inscription as lexicographically small as is possible. This is printed to stdout by locked stub code in the editor.
 
Sample Input 0
baabccd
 
Sample Output 0
1
 
Explanation 0
It takes one left-circular rotation to make the inscription as lexicographically small as is possible: baabccd → aabccdb. Thus, the function returns 1.
 
Sample Input 1
anadama
 
Sample Output 1
6
 
Explanation 1
It takes six left-circular rotations to make the inscription as lexicographically small as is possible: anadama → nadamaa → adamaan → damaana → amaanad → maanada → aanadam. Thus, the function returns 6.
 
Sample Input 2
ada
 
Sample Output 2
2
 
Explanation 2
It takes two left-circular rotations to make the inscription as lexicographically small as is possible: ada → daa → aad. Thus, the function returns 2.
-}

import Data.List
import Data.Maybe

shift :: [a] -> Int -> [a]
shift l n = drop n l  ++ take n l

allRotations :: [a] -> [[a]]
allRotations l = [ shift l i | i <- [0 .. (length l) -1]]

-- slow. 6/14 tests passed
minRotate inscription =
  let rotations = allRotations inscription
      small = head $ sort rotations
      index = fromMaybe 0 $ findIndex (==small) rotations
  in index
