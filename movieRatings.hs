{-
Alexa loves movies and maintains a list of negative and/or positive integer ratings for the n movies in her collection. She's getting ready for a film festival and wants to choose some subsequence of movies from her collection to bring such that the following conditions are satisfied:
The collective sum of their ratings is maximal.
She must go through her list in order and cannot skip more than one movie in a row. In other words, she cannot skip over two or more consecutive movies. For example, if ratings = [-1, -3, -2], she must include either the second number or the first and third numbers to get a maximal rating sum of -3.
 
Complete the maximizeRatings function in the editor below. It has one parameter: an array of integers, ratings, denoting the respective ratings for each movie. The function must return an integer denoting the maximum possible rating sum for Alexa's chosen subsequence of movies without reordering them.
 
Input Format
Locked stub code in the editor reads the following input from stdin and passes it to the function:
The first line contains an integer, n, denoting the number of movie ratings in ratings.
Each line i of the n subsequent lines (where 0 ≤ i < n) contains an integer describing ratingsi.
 
Constraints
1 ≤ n ≤ 105
-1000 ≤ ratingsi ≤ 1000, where 0 ≤ i < n
 
Output Format
The function must return an integer denoting the maximum possible rating sum for Alexa's chosen subsequence of movies without reordering them. This is printed to stdout by locked stub code in the editor.
 
Sample Input 0
5
9
-1
-3
4
5
 
Sample Output 0
17
 
Explanation 0
ratings = [9, -1, -3, 4, 5]
Alexa picks the bolded items in ratings = [9, -1, -3, 4, 5] to get maximum rating = 9 + -1 + 4 + 5 = 17. Thus, we return 17 as our answer.
 
Sample Input 1
5
-1
-2
-3
-4
-5
 
Sample Output 1
-6
 
Explanation 1
Alexa picks the bolded items in ratings = [-1, -2, -3, -4, -5] to get maximum rating = -2 + -4 = -6. Thus, we return -6 as our answer.
-}

--problem broken on HR
