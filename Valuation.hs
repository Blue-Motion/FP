module Valuation(Valuation) where

import Types
import Data.List
import Data.Tuple

type Valuation = [(Name, Integer)]

--list comprehension to the max :)

valuations :: [(Name,Domain)] -> [Valuation]
valuations [] = []
--valuations ((n,d):nds) = ([(n,y) | y <- d])
valuations v = [[((fst x),( head(snd x))) | x <- v]]