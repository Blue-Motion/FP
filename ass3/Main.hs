import Data.Char
import System.IO
import Data.List

import Expression

--Couldn't get split(on) to work

removeWhiteSpace :: String -> String
removeWhiteSpace s = filter (not.isSpace) s

readInput :: IO String
readInput = do input <- hGetContents stdin
               return (removeWhiteSpace input)

main :: IO ()
main = do input <- readInput
          putStrLn ( (show . takeCSPitems . splitCSP) input)
          return ()

splitCSP :: String -> (String, String)
splitCSP s = ((takeWhile (/= '}') (tail (dropWhile (/= '{') s))) , (takeWhile (/= '}') (tail (dropWhile (/= '{') (tail (dropWhile (/= '{') s))))))

takeCSPitems :: (String,String) -> ([String],[String])
takeCSPitems (d,c) = (addItem d (numCSPitems d), addItem c (numCSPitems c))
  where addItem _ 0 = []
        addItem ds n = (takeWhile (/= ',') ds):(addItem (tail (dropWhile (/= ',') ds))) (n-1)
        numCSPitems ds = length (filter (== ',') ds) + 1

