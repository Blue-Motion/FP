import Data.List
-- Exercise 1

-- Exercise 2
mults :: Integer -> [Integer]
mults n = [x * n | x <- [1..]]
--

--beschouw de lijst van mults hierboven als element, gebruik een functie die van elk van de genereerde lijsten van x:xs telkens de eerste neemt en map die op de lijst van lijsten. gebruik dan foldr ++ op deze lijsten om van de lijst van lijsten een lijst van elementen te maken.			
multiples :: [Integer] -> [Integer]
multiples [] = []
multiples (x:xs) = mults x : multiples xs

--multsum :: Integer -> [Integer] -> Integer
--multsum n (x:xs)  = 