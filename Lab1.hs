--Exersise 1

isPrime :: Integer -> Bool
isPrime 1 = True
isPrime 2 = True
isPrime x = not (even x) && notElem True [divides x y | y <- [2..div x 2], odd y]

divides :: Integer -> Integer -> Bool
divides x y = (&&) ((>) x y) ((==) 0 (mod x y))

listPrimes :: Integer -> [Integer]
listPrimes 2 = [2]
listPrimes x = listPrimes (x - 1) ++ [x]