import           Data.Char

replace :: Char -> Char -> String -> String
replace fnd rep s = foldr (\c s2 -> if(c==fnd) then rep : s2 else c : s2) "" s

newLineToSemicolon :: String -> String
newLineToSemicolon s = replace '\n' ';' s

getUserLines :: IO String                      -- optional type signature
getUserLines = go ""
    where go contents = do
            line <- getLine
            if line == ""
            then return contents
            else go (contents ++ line ++ "\n")     -- add a newline

main :: IO ()
main = do
  linesIn <- getUserLines
  let linesOut = newLineToSemicolon linesIn
  putStrLn linesOut


