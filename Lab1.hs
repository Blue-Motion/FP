--import Test.QuickCheck
--All functions are intentionally written in prefix notation. One of the goals was to reduce the number of parenthesis, but haskell somehow still needs them at points it shouldn't


--Exersize 1

--a
primes = 2 : filter (null . tail . primeFactors) [3,5..]

primeFactors n = factor n primes
  where
    factor n (p:ps) 
        | p*p > n        = [n]
        | mod n p == 0 = p : factor (div n p) (p:ps)
        | otherwise      =     factor n ps

isPrime :: Integer -> Bool
isPrime n = elem n primes

listPrimes :: Int -> [Integer]
listPrimes x = take x primes

--up to 2^15, the following is reasonably fast, above it takes forever.
--b
cntPrimes :: Integer -> Int
cntPrimes x = length (listPrimes x)

--c
oddPspTO :: Integer -> Integer -> [Integer]
oddPspTO a upb = [n | n <- [3,5..upb], a^(n-1) `mod` n == 1, not (isPrime n)]

--works somehow
expmod :: Integer -> Integer -> Integer -> Integer
expmod a e n
       | (==) e 1 = mod a n
       | even e = expmod ((*) a a) (div e 2) n
       | otherwise = mod ((*) a (expmod a ((-) e 1) n)) n

impOddPspTO :: Integer -> Integer -> [Integer]
impOddPspTO a upb = [n | n <- [3,5..upb], (==) (expmod a ((-) n 1) n) 1, not (isPrime n)]

ord1 :: Integer -> Integer -> Integer
ord1 a p = ord a (a `mod` p) 1 p
     where ord a e k p = if e == 1 then k else ord a (a*e `mod` p) (k+1) p

order :: Integer -> Integer -> Integer
order a p = product [x | x <- primeFactors (p-1), notElem x (primeFactors a)]

prop x y = ord1 x y == order x y

factor :: Integer -> [Integer]
factor n = [y | y <- listPrimes (div n 2), mod n y == 0]

--oddPspTO :: Integer -> Integer -> [Integer]
--oddPspTO a upb = 

 
