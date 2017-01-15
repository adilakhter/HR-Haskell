{-
You will be provided with a fragment of HTML code. Your task is to identify unique potential domain names from the links or URLs which are present in that code fragment.
For example, if the link http://www.hackerrank.com/contest is present in the markup, you should detect the domain: hackerrank.com. If second-level or higher-level domains are present in the markup, all of them need to be treated as unique. For instance if the links http://www.xyz.com/news, https://abc.xyz.com/jobs, http://abcd.xyz.com/jobs2 are present in the markup then [xyz.com, abc.xyz.com, abcd.xyz.com] should all be identified as unique domains. The generic prefixes "www.", "ww2." and "web." should be trimmed off if present.
 
Constraints
1 ≤ N ≤ 1700

Input Format
An Integer N giving the number of lines in the HTML fragment which follows. The HTML fragment should then be read from the following N lines.

Output Format
One line, containing the list of detected domains, separated by semi-colons, in lexicographical order. Do not leave any leading or trailing spaces either at the ends of the line, or before and after the individual domain names.
 
Sample Input #00
10
^ ["Train (noun)"](http://www.askoxford.com/concise_oed/train?view=uk). (definition – Compact OED). Oxford University Press. Retrieved 2008-03-18.
^ Hello
^ World
^ C is a programming language.
^ 
^ Atchison, Topeka and Santa Fe Railway (1948). Rules: Operating Department. p. 7.
^ [Hydrogen trains](http://www.hydrogencarsnow.com/blog2/index.php/hydrogen-vehicles/i-hear-the-hydrogen-train-a-comin-its-rolling-round-the-bend/)
^ [Vehicle Projects Inc. Fuel cell locomotive](http://www.bnsf.com/media/news/articles/2008/01/2008-01-09a.html)
^ Central Japan Railway (2006). Central Japan Railway Data Book 2006. p. 16.
^ ["Overview Of the existing Mumbai Suburban Railway"](http://web.archive.org/web/20080620033027/http://www.mrvc.indianrail.gov.in/overview.htm). _Official webpage of Mumbai Railway Vikas Corporation_. Archived from [the original](http://www.mrvc.indianrail.gov.in/overview.htm) on 2008-06-20. Retrieved 2008-12-11.
 
Sample Output #00
archive.org;askoxford.com;bnsf.com;hydrogencarsnow.com;mrvc.indianrail.gov.in
 
Explanation #00
archive.org;askoxford.com;bnsf.com;hydrogencarsnow.com;mrvc.indianrail.gov.in are the only valid domains

import Data.Ratio
import Text.Regex (subRegex, mkRegex)
import Text.Regex.Posix

s = "https?://w*\\.?(([a-zA-Z0-9])*.?)([a-zA-Z]*)"
s' = "[a-zA-Z0-9][a-zA-Z0-9-_]{0,61}[a-zA-Z0-9]{0,1}\\.([a-zA-Z]{1,6}|[a-zA-Z0-9-]{1,30}\\.[a-zA-Z]{2,3})"
parse :: String -> [String]
parse str = let
  (_, _, _, s) = str =~ s' :: (String,String,String,[String])
  in s

-}


import Data.Maybe
import Data.List
import qualified Network.URI as URI
import Text.Regex (splitRegex, mkRegex, subRegex)

tokenize :: String -> [String]
tokenize = splitRegex (mkRegex "[[:space:],]")

isURI s = case URI.parseURI s of 
  Nothing -> False
  Just _ -> True
  
getDomain s = let
  uri = URI.parseURI s >>= URI.uriAuthority
  domain = fmap URI.uriRegName uri
  in subRegex (mkRegex "www\\.") (fromMaybe "" domain) ""
  
getDomains s = map getDomain $ filter isURI $ tokenize s

printDomains arr = let
  outputs = arr >>= getDomains
  in mapM_ putStrLn $ sort outputs

