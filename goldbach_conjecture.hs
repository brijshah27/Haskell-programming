--Returns two numbers from oddComposite numbers which disproves goldbach conjecture.
goldbach :: [Integer]
goldbach = take 2 [n | n <- [i | i <- oddsFrom3, not(isPrime i)], failConjecture n]

--Taking all odd numbers
oddsFrom3 = [3, 5..]

--Return True if number passed in argument is member of primeList.
isPrime :: Integer -> Bool
isPrime n = n `elem` primeList n

--Returns the list of prime numbers which are less or equal to the number passed in the argument.
primeList :: Integer -> [Integer]
primeList n = [d | d <- takeWhile (\x -> x <= n) primes]

-- Returns list of all prime numbers.
primes :: [Integer]
primes = 2 : [n | n <- oddsFrom3, null (primeDivisors n)]

-- Returns list of primedivisors of the argument.
primeDivisors :: Integer -> [Integer]
primeDivisors n = [d | d <- takeWhile (\x -> x^2 <= n) primes, n `mod` d == 0]

--Return True if we can find Prime number which satisfy (g-p)/2 is not a perfect square for value passed in argument.
failConjecture :: Integer -> Bool
failConjecture g = null([p | p <- primeList g, isSquare ((g - p) `div` 2)])

-- Return True if integer passed in argument is a perfect square.
isSquare :: Integer -> Bool
isSquare n = (floor (sqrt (fromIntegral n)) ^ 2 == n)