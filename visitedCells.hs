{-
Babai is standing in the top left cell (1,1) of an N × M grid. (The grid has N rows and M columns.) Initially, he is facing to the right cell. He moves in the table in the following manner:
 
If the cell in front of him is within the grid and not previously visited, he moves one step forward into it, and turns to his right.
Otherwise he turns to his right, and tries step 1 again.
If he has no right turns remaining (he has tried step 2 four times), then he stops.

He moves around the grid and visits as many cells as he can. Your task is to find out the number of cells that he visits before he stops.

Here is a sample of Babai's steps on a 9x9 grid. The value at each cell denotes the sequence in which he visits it.
  1   2  55  54  51  50  47  46  45
  4   3  56  53  52  49  48  43  44
  5   6  57  58  79  78  77  42  41
  8   7  60  59  80  75  76  39  40
  9  10  61  62  81  74  73  38  37
12  11  64  63  68  69  72  35  36
13  14  65  66  67  70  71  34  33
16  15  20  21  24  25  28  29  32
17  18  19  22  23  26  27  30  31 

Constraint:
1 ≤ N, M ≤ 100
 
Input Format:
The function "totalCellsVisited" contains #2 integers as its arguments N and M respectively.

Output Format:
Return a single integer which denotes the required result.

Sample input #00:
3
3
Sample output #00:
9
Explanation #00:
The grid cells are visited in the sequence indicated by the integers written inside them below.never visited.
 1  2  9
 4  3  8
 5  6  7

Sample input #01:
7
4
Sample output #01:
18
Explanation #01:
The grid cells are visited in the sequence indicated by the non-zero integers written inside them below. Zeroes indicate the cells never visited.
 1  2  0  0
 4  3  0  0
 5  6  0  0
 8  7  0  0
 9 10  0  0
12 11 16 17
13 14 15 18

function totalCellsVisited( $n,  $m) {
    if ($n & 1) {
        if ($m & 1) {
            return $n * $m;
        }
        else {
            return ($n * 2) + (($m * 2) - 4);
        }
    }
    else {
        return $n * 2;
    }
}
-}

totalCellsVisited :: Int -> Int -> Int
totalCellsVisited n m
  | n `mod` 2 == 0 = n * 2
  | (n `mod` 2 == 1) && (m `mod` 2 == 0) = (n * 2) + (m * 2) - 4
  | otherwise = n * m

