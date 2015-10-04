--primes
primes :: [Integer]
--primes = 2 : filter (null . tail . primeFactors) [3,5..]
primes = 2 : filter ((==1) . length . primeFactors) [3,5..]

primeFactors n = factor n primes
  where
    factor n (p:ps) 
      | p*p > n = [n]
      | mod n p == 0 = p : factor (div n p) (p:ps)
      | otherwise = factor n ps

divisors :: Integer -> [Integer]
divisors n = rmdup (primeFactors n)
 where rmdup [] = []
       rmdup (f:fs)
        | fs == [] = [f]
        | otherwise =  f:(rmdup (dropWhile (==f) fs))

isPrime :: Integer -> Bool
isPrime 0 = False
isPrime n = length (primeFactors n) == 1

listPrimes :: Integer -> [Integer]
listPrimes x = takeWhile (< x) primes

sort :: Ord a => [a] -> [a]
sort = inSort

--quicksort is no use at all, the list is sorted quite well beforehand
quicksort :: Ord a => [a] -> [a]
quicksort []     = []
quicksort (p:xs) = (quicksort lesser) ++ [p] ++ (quicksort greater)
    where
        lesser  = filter (< p) xs
	greater = filter (> p) xs

inSort :: Ord a => [a] -> [a]
inSort [] = []
inSort (x:xs) = ins x (inSort xs)

ins :: Ord a => a -> [a] -> [a]
ins x []  =  [x]
ins x (y:ys) | x <= y     =  x : y : ys
 | otherwise  =  y : ins x ys


--Ex1
--Smallest multiple
--the second half of the range is always twice the first half, if we compute the product of the second half, is it always divisible by all first half elements, 

smallestMultiple n = div (prod [(h+1)..n]) (prod [3..(h-1)])
   where h = div n 2
         prod = foldr (*) 1

--Ex2
--sum of multiples

mults :: Integer -> [Integer]
mults n = [x * n | x <- [1..]]

--way to slow, need to find a heuristic for sorting
multiples :: [Integer] -> [Integer]
multiples m = sort (foldr (++) [] (map mults m))

multsum :: Integer -> [Integer] -> Integer
multsum n xs = sum (takeWhile (<n) (multiples xs))

--Ex3
--Distinct powers

powers :: Integer -> [Integer]
powers a = [a^n | n <- [2..]]

distinctPowers :: Integer -> Int -> [Integer]
--distinctPowers a b = concat (transpose (map ((take (b-1)) . powers) [2..a]))
distinctPowers a b =  foldr (++) [] (map ((take (b-1)) . powers) [2..a])

--Ex4
--Palindromic composite
--without duplicate removal, needs sorting,but is too slow
numberOfPalindromicComposites :: Integer -> Int
numberOfPalindromicComposites n = length  (filter (isPalindromeBelow) (composites n))
  where isPalindromeBelow c = c<n && isPalindrome c
        composites n = foldr (++) [] [map (*x) ps | x <- ps]
        ps = listPrimes (div n 2)

isPalindrome :: (Show a) => a -> Bool
isPalindrome n = (reverse . show) n == show n

--Ex5
--Last n Digits
--works like a charm, except a kill on calculating nsum, must be a shortcut
lastDigits :: Integer -> Integer -> [Integer]
lastDigits n d = lDig nsum d
  where nsum = foldr (+) 0 [x^x | x <- [1..n]]
        --trimexp _ 0 = 1
	--trimexp a e = mod (a * (trimexp a (e-1))) (10^d)
        lDig _ 0 = []
        lDig m e = (lDig (div m 10) (e-1)) ++ [(mod m 10)]

--what the...! its even slower to trim the thing down to 10^d, probably because of recursion
trimexp :: Integer -> Integer -> Integer
trimexp _ 0 = 1
trimexp a e = mod (a * (trimexp a (e-1))) (10^10)
        

--Ex6
--Factorial sums

--sumsg :: Integer -> Integer
--sumsg n = sum (map sg [1..n])
--      where
--	f n = sum(map fac (toDigits n))
--	g i = head [x | x <- [1..], sf x == i]
--	s fun = sum . toDigits . fun
--      	sg = s g
--      	sf = s f

fac :: Integer -> Integer
fac n = product [1..n]

--Ex7
--Repetitive reciprocals
maxRepRec :: Integer -> Integer -> Integer
maxRepRec m n = 0

repRec :: Integer -> Integer
repRec n = findRep nList [] []
  where findRep [] p r = []
        findRep a p r
	  | take rLen a == r = rLen + findRep (drop rLen a) p [r ++ r]
	  | take (length p) nList == p = findRep (tail a) (head a) 
        nList = show (1.0 / (fromIntegral n))
	rLen = length r