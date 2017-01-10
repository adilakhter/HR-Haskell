{-
You have to complete the given function pointsBelongToTriangle that takes integers x1, y1, x2, y2, x3, y3, p1, q1, p2 and q2 as its arguments and returns one integer from {0, 1, 2, 3, 4}.
 
(x1, y1) is the coordinate of point A.
(x2, y2) is the coordinate of point B.
(x3, y3) is the coordinate of point C.
(p1, q1) is the coordinate of point P.
(p2, q2) is the coordinate of point Q.
 
The return value is:
0: If points A, B and C don't form a triangle.
1: If point P "belongs to the triangle" ABC but point Q doesn't.
2: If point Q belongs to the triangle ABC but point P doesn't.
3: If both points P and Q belong to the triangle ABC.
4 : If neither point P nor point Q belong to the triangle.
 
Note
A point "belongs to a triangle" if it lies on or inside the triangle.
 
Constraints
0 <= x1, y1, x2, y2, x3, y3, p1, q1, p2, q2 <= 2000
 
Sample Input 0
A = (0, 0), B = (2, 0), C = (4, 0), P = (2, 0) and Q = (4, 0)
 
Sample Output 0
0
 
Explanation
Points A, B and C lies on the same line, so it is not possible to form a triangle, so the answer is 0.
 
Sample Input 1
A = (3, 1), B = (7, 1), C = (5, 5), P = (3, 1) and Q = (0, 0)
 
Sample Output 1
1
 
Explanation
Points A, B and C form a triangle. Point P belongs to the triangle but point Q doesn't, so the answer is 1.
 
Sample Input 2
A = (3, 1), B = (7, 1), C = (5, 5), P = (1, 1) and Q = (4, 3)
 
Sample Output 2
2
 
Explanation
Points A, B and C form a triangle. Point Q belongs to the triangle but point P doesn't, so the answer is 2.
 
Sample Input 3
A = (3, 1), B = (7, 1), C = (5, 5), P = (5, 2) and Q = (6, 3)
 
Sample Output 3
3
 
Explanation
Points A, B and C form a triangle. Both the points P and Q belong to the triangle, so the answer is 3.
 
Sample Input 4
A = (3, 1), B = (7, 1), C = (5, 5), P = (1, 1) and Q = (2, 2)
 
Sample Output 4
4
 
Explanation
Points A, B and C form a triangle. Neither point P belongs to the triangle nor point Q, so the answer is 4.
-}

type Point = [Int]
type Triangle = ([Point], Int)
type Edge = ([Point], Int)

dimension :: Point -> Int
dimension = length

orientation :: [Point] -> Int
orientation = signum . det

makeTriangle :: [Point] -> Triangle
makeTriangle t = (t, orientation t)

minors :: [a] -> [[a]]
minors [] = []
minors (x:xs) = xs : map (x:) (minors xs)

det :: [[Int]] -> Int
det [[x]] = x
det xss = foldr1 (-) (zipWith (*) col1 (map det (minors cols)))
  where col1 = map head xss
        cols = map tail xss

edges :: Triangle -> [Edge]
edges (us, b) = zip (minors us) (cycle [b,-b])

inside :: Triangle -> Point -> Bool
inside tri p = and [ 0 <= b * orientation (p:us) | (us,b) <- edges tri]

pointsBelongToTriangle x1 y1 x2 y2 x3 y3 p1 q1 p2 q2 =
  let points = [[x1,y1,1],[x2,y2,1],[x3,y3,1]]
      tri = makeTriangle points
      lin = 0 == orientation points
      p = [p1,q1,1]
      q = [p2,q2,1]
      p' = inside tri p
      q' = inside tri q
  in case (p',q',lin) of
    (False, False, False) -> 4
    (True, True, False) -> 3
    (False, True, False) -> 2
    (True, False, False) -> 1
    (_,_,True) -> 0
    

