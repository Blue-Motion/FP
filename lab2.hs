import Data.List
-- Exercise 1 --seems to work, is really fast
--de bovenst helft van de reeks bevat alle dubbelen van de onderste helft, het product van de bovenste helft is dus ook deelbaar door elk getal in de onderste helft
--het interval 3..h-1 is me nog niet helemaal helder, maar dergelijke berekening is de enige manier op zo snel een antwoord te kunnen genereren
smallestMultiple n = div (product [(h+1)..n]) (product [3..(h-1)])
		 where h = div n 2


rmdups :: (Ord a) => [a] -> [a]
--rmdups = map head . group . quicksort
rmdups = quicksort

quicksort :: Ord a => [a] -> [a]
quicksort []     = []
quicksort (p:xs) = (quicksort lesser) ++ [p] ++ (quicksort greater)
    where
        lesser  = filter (< p) xs
        greater = filter (> p) xs

-- Exercise 2
mults :: Integer -> [Integer]
mults n = [x * n | x <- [1..]]
--

--beschouw de lijst van mults hierboven als element, gebruik een functie die van elk van de genereerde lijsten van x:xs telkens de eerste neemt en map die op de lijst van lijsten. gebruik dan foldr ++ op deze lijsten om van de lijst van lijsten een lijst van elementen te maken.
--hmm, bovenstaande werkt niet. dan is de sortering toch niet goed.

--mults mappen op de lijst van input

--ex2, concat is implemented as foldr ++ []
--dubbelen moeten er nog uit. volgorde klopt ook niet.	Bovenstaande sort werkt niet, er moet een sort komen die oneindige lijsten kan handlen, zoiets als tijdens college behandeld	
multiples :: [Integer] -> [Integer]
multiples m = concat (transpose (map mults m))

multsum :: Integer -> [Integer] -> Integer
multsum n xs = sum (takeWhile (<n) (multiples xs))

--ex3
powers :: Integer -> [Integer]
powers a = [a^n | n <- [2..]]

--zelfde als hierboven, dubbelen en volgorde, verder is het nog neit al te vlot, alhoewel hij wel volle bak aan het outputten is
distinctPowers :: Integer -> Int -> [Integer]
--distinctPowers a b = concat (transpose (map ((take (b-1)) . powers) [2..a]))
distinctPowers a b = quicksort ( concat (map ((take (b-1)) . powers) [2..a]))

--ex4
--nothing yet


--toDigits takes an integer and produces the digits of that integer in a list
toDigits :: Integer -> [Integer]
toDigits x
	 | x < 10 = [x]
	 | otherwise = toDigits (div x 10) ++ [(mod x 10)]

--ex5
--als n groter is dan 10^d, dan kan je gewoon de bovenkant afkappen, klopt nog niet veel van..
lastDigits :: Integer -> Integer -> [Integer]
lastDigits n d = toDigits (mod (sum [ x^x | x <- [1..(mod n (10^d))]]) (10 ^ d))
   

--ex6 --complete

sumsg :: Integer -> Integer
sumsg n = sum (map sg [1..n])
      where
	f n = sum(map fac (toDigits n))
	g i = head [x | x <- [1..], sf x == i]
	s fun = sum . toDigits . fun
      	sg = s g
      	sf = s f

fac :: Integer -> Integer
fac n = product [1..n]

--ex8
type Sudoku = [String]

--isCorrectSudoku :: Sudoku -> Boolean
--isCorrectSudoku (x:xs)
--	| 