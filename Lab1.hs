-- Thom Carretero Seinhorst (s1898760)
-- Bart Offereins (s2255243)

import Test.QuickCheck
--All functions are intentionally written in prefix notation. One of the goals was to reduce the number of parenthesis, but haskell somehow still needs them at points it shouldn't

--Exersize 1

--a
--At first, there was a more naive approach to find primes, but after 
--the latest lecture and the later exersizes, we replaced it with a way 
--that uses smaller primes to find new ones.
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

--up to 2^20, the following is reasonably fast, above that it takes forever.
--b
cntPrimes :: Integer -> Int
cntPrimes = length . listPrimes

--c
oddPspTO :: Integer -> Integer -> [Integer]
oddPspTO a upb = [n | n <- [3,5..upb], a^(n-1) `mod` n == 1, (not . isPrime) n]

--d
expmod :: Integer -> Integer -> Integer -> Integer
expmod a e n
       | (==) e 1 = mod a n
       | even e = expmod ((*) a a) (div e 2) n
       | otherwise = mod ((*) a (expmod a ((-) e 1) n)) n

impOddPspTO :: Integer -> Integer -> [Integer]
impOddPspTO a upb = [n | n <- [3,5..upb], (==) (expmod a ((-) n 1) n) 1, (not . isPrime) n]

---2

--n = p.q
--p = prime
--q > 1
-- 1<= a < p



org_order :: Integer -> Integer -> Integer
org_order a p = ord a (mod a p) 1 p
  where ord a e k p
          | not (isPrime p) || a < 1 || a >= p = 0 --set bounds for quickcheck
          | e == 1 = k
          | otherwise = ord a (mod (a*e) p) (k+1) p


--how to know what factors we need?
order :: Integer -> Integer -> Integer
order a p
  | not (isPrime p) || a < 1  || a >= p = 0 --set bounds for quickcheck
  | otherwise = ord a p (productSet (primeFactors (p-1)))
  where ord a p (k:ks)
         | expmod a k p == 1 = k
         | otherwise = ord a p ks
        productSet :: [Integer] -> [Integer]
        productSet []     = [1]
        productSet (x:xs) = merge (productSet xs) (map (*x) (productSet xs)) --for every factor, take the product with all factors and sort them into 1 list 

merge :: [Integer] -> [Integer] -> [Integer]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
    | x == y    = merge (x:xs) ys
    | x >  y    = y : merge (x:xs) ys
    | otherwise = x : merge xs (y:ys)

prop_order a p = ord1 a p == order a p

ord1 :: Integer -> Integer -> Integer
ord1 a p
   | not (isPrime p) || a < 1  || a >= p = 0 --set bounds for quickcheck
   | otherwise = ord a (a `mod` p) 1 p
      where ord a e k p = if e == 1 then k else ord a (a*e `mod` p) (k+1) p

--oddPspTO2 :: Integer -> Integer -> [Integer]
--oddPspTO2 a upb = [n | n <- [3,5..upb],  ]

 

