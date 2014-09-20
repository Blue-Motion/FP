All functions are intentionally written in prefix notation. One of the goals was to reduce the number of parenthesises, but haskell somehow still needs them at points it shouldn't

--Exersize 1

--a
isPrime :: Integer -> Bool
isPrime 1 = True
isPrime 2 = True
isPrime x = (&&) (not (even x)) (notElem True [divides x y | y <- [2..div x 2], odd y])

divides :: Integer -> Integer -> Bool
divides x y = (&&) ((>) x y) ((==) 0 (mod x y))

listPrimes :: Integer -> [Integer]
listPrimes x = [y | y <- [2..x], isPrime y]

--up to 2^15, the following is reasonably fast, avobe it takes forever.
--b
cntPrimes :: Integer -> Int
cntPrimes x = length (listPrimes x)

--c
oddPspTO :: Integer -> Integer -> [Integer]
oddPspTO a upb = [n | n <- [3,5..upb], a^(n-1) `mod` n == 1, not (isPrime n)]

--not working properly yey
expmod :: Integer -> Integer -> Integer -> Integer
expmod a e n
       | e == 1 = mod a n
       | (>) a n = expmod (mod a n) e n
       | even e = expmod ((*) a a) (div e a) n
       | otherwise = a * expmod a ((-) e 1) n