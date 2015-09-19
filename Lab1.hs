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
  where ord _ 1 k _ = k
        ord a e k p = ord a (mod (a*e) p) (k+1) p


--how to know what factors we need?
order :: Integer -> Integer -> Integer
order a p = ord a p (filter (/=a ) (primeFactors (p-1)))
  where ord a p (k:ks)
         | expmod a k p == 1 = k
         | otherwise = ord a p ((k * head ks):tail ks)

prop_order a p = org_order a p == order a p








ord1 :: Integer -> Integer -> Integer
ord1 a p = ord a (a `mod` p) 1 p
     where ord a e k p = if e == 1 then k else ord a (a*e `mod` p) (k+1) p

-- no clue what part of the subset is actually used, seems to be something with the product 
-- of the first prime and the product of the tail, but there is most of the time 
-- a (prime) factor difference between 0rd1 and order. It can't possibly be a permutation 
-- of the factors list because calculating permutaions takes way longer than just iterating over e.
--order :: Integer -> Integer -> Integer
--order a p 
--        | even (mod a p) = product (divisors (p-1))
--        | otherwise = product (tail  (divisors (p-1)))
--
--      | product f:fs == (p-1) = product f:(dropWhile (==f) fs) 
--      | otherwise = product fs
--      	where f:fs = primeFactors (p-1)
prop x y = ord1 x y == order x y
 

--oddPspTO2 :: Integer -> Integer -> [Integer]
--oddPspTO2 a upb = [n | n <- [3,5..upb],  ]

 
