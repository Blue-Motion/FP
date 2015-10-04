--primes
primes :: [Integer]
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

--merges 2 ordered lists, removing doubles
--could maybe lose a pattern

merge :: (Ord a) => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
  | x < y = x : (merge xs (y:ys))
  | x == y = x : (merge xs ys)
  | otherwise = y : (merge (x:xs) ys)

--toDigits takes an integer and produces the digits of that integer in a list
toDigits :: Integer -> [Integer]
toDigits x
	 | x < 10 = [x]
	 | otherwise = toDigits (div x 10) ++ [(mod x 10)]

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

multiples :: [Integer] -> [Integer]
multiples m = foldr merge [] (map mults m)

multsum :: Integer -> [Integer] -> Integer
multsum n xs = foldr (+) 0 (takeWhile (<n) (multiples xs))

--Ex3
--Distinct powers

powers :: Integer -> [Integer]
powers a = [a^n | n <- [2..]]

distinctPowers :: Integer -> Int -> Int
distinctPowers a b =  length (foldr merge [] (map ((take (b-1)) . powers) [2..a]))

--Ex4
--Palindromic composite
--with duplicate removal, relies heavily on a fast primality test, which we did not manage to build last week
numberOfPalindromicComposites :: Integer -> Int
numberOfPalindromicComposites n = length (filter (isPalindromeBelow) (composites n))
  where isPalindromeBelow c = c<n && isPalindrome c
        composites n = foldr merge [] [map (*x) ps | x <- ps]
        ps = listPrimes (div n 3)

isPalindrome :: (Show a) => a -> Bool
isPalindrome n = show n == (reverse . show) n

--Ex5
--Last n Digits
--works like a charm, except a kill on calculating nsum, must be a shortcut
lastDigits :: Integer -> Integer -> [Integer]
lastDigits n d = toDigits nsum
  where nsum = foldr (+) 0 ([expmod x x (10^d) | x <- [1..n]])

--http://hackage.haskell.org/package/hS3-0.5.8/docs/src/Codec-Encryption-RSA-NumberTheory.html
expmod :: Integer -> Integer -> Integer -> Integer
expmod a x m
  | x == 0 = 1
  | x == 1 = mod a m
  | even x =
    let p = (expmod a (mod (div x 2) m) m)
                            in  mod (p^2) m
  | otherwise = mod (a * expmod a (x-1) m)  m 

        

--Ex6
--Factorial sums

sumsg :: Integer -> Integer
sumsg n = sum (map sg [1..n])
      where
	f n = sum(map fac (toDigits n))
	g i = head [x | x <- [1..], sf x == i]
	s fun = sum . toDigits . fun
      	sg = s g
      	sf = s f

fac :: Integer -> Integer
fac n = foldr (*) 1 [1..n]

--Ex7
--Repetitive reciprocals
maxRepRec :: Integer -> Integer -> Integer
maxRepRec m n = 0

--repRec :: Integer -> Integer
--repRec n = findRep nList [] []
--  where findRep [] p r = []
--        findRep a p r
--	  | take rLen a == r = rLen + findRep (drop rLen a) p [r ++ r]
--	  | take (length p) nList == p = findRep (tail a) (head a) 
--        nList = show (1.0 / (fromIntegral n))
--	rLen = 1