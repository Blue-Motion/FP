import Data.Char
import System.IO
import Data.List

import Compare
import Expression

--Couldn't get split(on) to work

removeWhiteSpace :: String -> String
removeWhiteSpace s = filter (not.isSpace) s

readInput :: IO String
readInput = do input <- hGetContents stdin
               return (removeWhiteSpace input)

main :: IO ()
main = do input <- readInput
          putStrLn ( (show . parseCSPitem . takeCSPitems . splitCSP) input)
          return ()

splitCSP :: String -> (String, String)
splitCSP s = ((takeWhile (/= '}') (tail (dropWhile (/= '{') s))) , (takeWhile (/= '}') (tail (dropWhile (/= '{') (tail (dropWhile (/= '{') s))))))

takeCSPitems :: (String,String) -> ([String],[String])
takeCSPitems (d,c) = (addItem d (numCSPitems d), addItem c (numCSPitems c))
  where addItem _ 0 = []
        addItem ds n = (takeWhile (/= ',') ds):(addItem (tail (dropWhile (/= ',') ds))) (n-1)
        numCSPitems ds = length (filter (== ',') ds) + 1

varDomain :: String -> (Name,Domain)
varDomain s = (takeWhile isAlpha s,[(read (takeWhile (isDigit) (dropWhile (not.isDigit) s)))..(read ((reverse . (takeWhile isDigit) . tail . reverse)  s ))])

parseCSPitem :: ([String],[String]) -> ([(Name,Domain)],[Comparison])
parseCSPitem (a,b) = (map varDomain a, map toComparison b)