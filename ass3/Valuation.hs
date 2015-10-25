module Valuation where

import Types
--import Data.List
--import Data.Tuple

type Valuation = [(Name, Integer)]

--list comprehension to the max :)

valuations :: [(Name,Domain)] -> [Valuation]
valuations [] = []
valuations ((x,y):[]) =  [[(x,v)] | v <- y]
valuations (x:xs) = concat [(map (y++) (valuations xs)) | y <- (valuations [x])]
