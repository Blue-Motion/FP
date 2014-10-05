import Data.List
-- Exercise 1

--lcm :: Integer -> Integer -> Integer
--lcm a b = 


-- Exercise 2
mults :: Integer -> [Integer]
mults n = [x * n | x <- [2..]]
--

--beschouw de lijst van mults hierboven als element, gebruik een functie die van elk van de genereerde lijsten van x:xs telkens de eerste neemt en map die op de lijst van lijsten. gebruik dan foldr ++ op deze lijsten om van de lijst van lijsten een lijst van elementen te maken.
--hmm, bovenstaande werkt niet. dan is de sortering toch niet goed.

--mults mappen op de lijst van input


--ex2, concat is implemented as foldr ++ []
--dubbelen moeten er nog uit. volgorde klopt ook niet.		
multiples :: [Integer] -> [Integer]
multiples m = concat (transpose (map mults m))


multsum :: Integer -> [Integer] -> Integer
multsum n xs = sum (takeWhile (<n) (multiples xs))

powers :: Integer -> [Integer]
powers a = [a^n | n <- [2..]]

--distinctPowers :: Integer -> Integer -> [Integer]
--distinctPowers a b = 


--toDigits takes an integer and produces the digits of that integer in a list
toDigits :: Integer -> [Integer]
toDigits x = if x < 10 then [x] else toDigits (div x 10) ++ [(mod x 10)]

--ex5
--als n groter is dan 10^d, dan kan je gewoon de bovenkant afkappen
lastDigits :: Integer -> Integer -> [Integer]
lastDigits n d = toDigits (mod (sum [ x^x | x <- [1..(mod n (10^d))]]) (10 ^ d))
	   

--ex6
--f :: Integer -> Integer
--f n = sum(map fac (toDigits n))

--s :: (Integer -> Integer) -> Integer -> Integer
--s fun = sum . toDigits . fun

--sf :: Integer -> Integer
--sf = sum . toDigits . f
--sf = s f


--g :: Integer -> Integer
--g i = head [x | x <- [1..], sf x == i]

--sg :: Integer -> Integer
--sg = s g

sumsg :: Integer -> Integer
sumsg n = sum (map sg [1..n])
      where
	f n = sum(map fac (toDigits n)
	g i = head [x | x <- [1..], sf x == i]
	s fun = sum . toDigits . fun
      	sg = s g
      	sf = s f

fac :: Integer -> Integer
fac n = product [1..n]