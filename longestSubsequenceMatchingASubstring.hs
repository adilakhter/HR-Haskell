{-
We define a subsequence of a string to be a sequence of one or more of the string's characters. For example, the subsequences of "abc" are "a", "b", "c", "ab", "bc", "ac", and "abc".
 
We define a substring of a string to be a contiguous segment of one or more of a string's characters. For example, the substrings of "abc" are "a", "b", "c", "ab", "bc", and "abc".
 
Complete the longestSubsequence function in the editor below. It has two parameters:
A string, x.
A string, y.
The function must return an integer denoting the length of the longest subsequence of string x that is also a substring of string y.
 
Input Format
Locked stub code in the editor reads the following input from stdin and passes it to the function:
The first line contains a single string, x.
The second line contains a single string, y.
 
Constraints
1 ≤ |x|, |y| ≤ 2000, where |x| and |y| are the respective lengths of strings x and y.
Strings x and y consist of lowercase English alphabetic letters (i.e., a to z) only.
 
Output Format
The function must return an integer denoting the length of the longest subsequence of string x that is also a substring of string y. This is printed to stdout by locked stub code in the editor.
 
Sample Input 0
hackerranks
hackers
 
Sample Output 0
7
 
Explanation 0
"hackers" is the longest subsequence of string x that is also a substring of y, so we return its length (i.e., 7) as our answer.
 
Sample Input 1
abc
aedace
 
Sample Output 1
2
 
Explanation 1
"ac" is the longest subsequence of string x that is also a substring of y, so we return its length (i.e., 2) as our answer.

-}

import Data.List

substring y = [t | i <- inits y, t <- tails i, not $ null t]
subsequence = tail . subsequences

--only passes 2/10 tests. timing out on this problem:
--x = pxmfjrmvkehafjpxrehkkqcqbjpcmxymsgnfdzzplkdaewzoteyavwwzcnbtsrxyccjxfmbwsfquqelpicmmvymatfvwpemabhlxpjxuywludjhkfwpyowvnkpupalimnnecvtesblatxnewkywvvohdbgfavjxumqhvkznutjpowuvhmnyvzbykuzmchbnlmuiavdfbcuutaqqgiwhjefmcapfisdtohavoputtnhzecalriymlnfabvtbkhtnpenxkbtubuyskwykpablacspjkanwlnxeuuksccptvtqwomusmvuygfdmbkftbdlwmmxeudbdknqudfcrsaefetouygyejfelfqoqvhfabprdbjcihqrzfdbqcafvoowjskqwzironkxxsqedgbycvhnuskhdkkgfpggahvuznqytlldquvbofbxafrxmnbaignazengaxngdobatpmqfzghlamzuoelwvajlvzbuoxwluxqhsmcj
--y = ohazmsexovixkuuneqnzdhhsweyjmrevqszglreqzacuzefaszzyzramuctxeusmzmtajezzfnrqmmmcyvrogrhntqwlbfpatgjxlweewaiaqidxrqplxdudscuqhrfjtqvksksnfmfhcodvghtkgzojpzytmdcimjzwaonnwmhmsaacnrblvvzhwbiokgziuvsfersuxiiydcfcvnkpbzljsqrqtgmhywkjxlxsixlxrwsnyypjkoxgtyczbouvojmfoqptnqfkvrynavuywnemedlvbvlafhorcfpqixphfwoybefcsbubegqmhcgyfbetfsyuqbadugfylowmzrifijkzlpawkewixgcfvqxapcyzpegrzrqczfdssgvspnjktlshhjqvvlkcmvwtwclpfwlwwulvfvmnnzldpiotcalpktbklalusufgbkrqgzdbagtqzlzealvq
longestSubsequence x y =
  let x' = subsequence x
      y' = substring y
  in foldl max 0 $ map length $ filter (flip elem $ y') x'
