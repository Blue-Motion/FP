module Valuation(Valuation) where

import Types
import Data.List
import Data.Tuple

type Valuation = [(Name, Integer)]

--list comprehension to the max :)

valuations :: [(Name,Domain)] -> [Valuation]
valuations [] = [[]]
valuations ((x,y):[]) =  [[(x,v)] | v <- y]
--valuations ((x,y):xs) = [(x,v) | v <- y]:(valuations xs)
--valuations xs = [[((fst n),v) | v <- (snd n)] | n <- xs] 
--valuations xs = [[((map fst xs),v) | v <- (concat (map snd xs))] | n <- xs] 
valuations (x:xs) = concat [(map (y++) (valuations xs)) | y <- (vals x)]
	   where vals (x,y) =  [[(x,v)] | v <- y]