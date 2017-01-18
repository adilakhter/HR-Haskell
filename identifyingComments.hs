{-
Your task is to write a program which
accepts as input a C, C++ or Java program on multiple lines of text, and
outputs only the comments from those programs.
 
Comments in C, C++ and Java programs
1. Single Line Comments
// this is a single line comment

int x = 1; // a single line comment after code
 
2. Multi Line Comments
/* This is one way of writing comments */

/* This is a multi-line comment. 
   It spans several lines.
   This is often more convenient for the programmer. */

/*
 * This is also a multi-line comment.
 */
 
Precautions
Do not add any leading or trailing spaces. 
Do not alter the line break structure of multi-line comments (e.g by collapsing multiple lines into one.)
 
You should, however, remove any white-space characters that precede a comment.
 
Input Format
Each test case will be the source code of a program written in C, C++ or Java.
 
Constraints
The source code will have no more than 200 lines of text.
 
Output Format
From the program given to you, remove everything other than the comments.
 
Sample Input
/*This is a program to calculate area of a circle after getting the radius as input from the user*/  
#include <stdio.h>
int main()  
{  
   double radius,area;//variables for storing radius and area  
   printf("Enter the radius of the circle whose area is to be calculated\n");  
   scanf("%lf",&radius);//entering the value for radius of the circle as float data type  
   area=(22.0/7.0)*pow(radius,2);//Mathematical function pow is used to calculate square of radius  
   printf("The area of the circle is %lf",area);//displaying the results  
}  
/*A test run for the program was carried out and following output was observed  
If 50 is the radius of the circle whose area is to be calculated
The area of the circle is 7857.1429*/
 
Sample Output
/*This is a program to calculate area of a circle after getting the radius as input from the user*/
//variables for storing radius and area
//entering the value for radius of the circle as float data type
//Mathematical function pow is used to calculate square of radius
//displaying the results
/*A test run for the program was carried out and following output was observed
If 50 is the radius of the circle whose area is to be calculated
The area of the circle is 7857.1429*/
-}


import Control.Monad
import Text.Parsec
import Control.Applicative hiding ((<|>), many)


type Parser = Parsec String ()

eol :: Parser ()
eol = void (char '\n') <|> eof

singleLine :: Parser String 
singleLine = do
  try $ many $ noneOf "/\n" --breaks on div: /
  string "//"
  comment <- many1 $ noneOf "\n"
  endOfLine
  return $ "//" ++ comment ++ "\n"

multiLine :: Parser String
multiLine = do
  string "/*"
  comment <- many1 anyChar
  string "*/"
  endOfLine
  return $ "/*" ++ comment ++ "*/" ++ "\n"

codeLine :: Parser String
codeLine = do
  code <- many $ noneOf "\n"
  endOfLine
  return ""

comments :: Parser String
comments = let
  l = (try singleLine) <|> multiLine <|> codeLine
  in fmap concat $ many1 l

main = do
  s <- getLine
  let output = parse comments "" (s++"\n")
  --print output
  putStr $ either (const "Invalid") id output
