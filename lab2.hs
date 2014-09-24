import Data.List
-- Exercise 1

-- Exercise 2
mults :: Integer -> [Integer]
mults n = [x * n | x <- [1..]]
--
multiples :: [Integer] -> [Integer]
multiples [] = []
multiples (x:xs) = sort ((mults x) ++ (multiples xs))

multsum :: Integer -> [Integer] -> Integer
multsum n (x:xs)  = 