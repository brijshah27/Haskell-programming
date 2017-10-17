-- The first version uses straightforward recursion.
-- This myMap1 implementation is a tail recursive map function equivalent
-- where function 'f' is applied on each head/first element of the list
-- and then myMap1 is recursively called on the remaining tail of the list.
-- myMap1 _ [] is the edge condition where any function (denoted as _)
-- applied on an empty list will result in an empty list.
myMap1 :: (a -> b) -> [a] -> [b]
myMap1 _ [] = []
myMap1 f (x:xs) = f x : myMap1 f xs

--myMap1 f (x:xs) = (f x) : myMap1 f xs


-- The second version implements map in terms of foldr.
-- This myMap1 implementation is a map function equivalent using foldr
-- where the 2 parameters labmda function is applied on each element of the list
-- where the list element is the first parameter and ":[]" is the second parameter
-- The lambda function applies the map function f on each element and keeps
-- adding f(x) to a result list.
myMap2 :: (a -> b) -> [a] -> [b]
myMap2 f = foldr (\x accum -> (f x : accum)) []
--myMap2 f xs = foldr (\x accum -> (f x : accum)) [] xs

-- 1. myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
-- Implements myZipWith using map, uncurry and zip functions
-- Step 1. zip combines 2 lists into a list of tuples
-- Step 2. map applies the uncurried function f on each tuple from list
myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith f a b = map (uncurry f) (zip a b)
-- From http://learnyouahaskell.com
--myZipWith _ [] _ = []  
--myZipWith _ _ [] = []  
--myZipWith f (a:as) (b:bs) = f a b : myZipWith f as bs
testMyZipWith :: [Integer]
testMyZipWith = myZipWith (+) [1,2,3] [4,5,6]

-- 2. myFoldl :: (b -> a -> b) -> b -> [a] -> b
-- The edge condition of recursion occurs when any function is applied
-- on a empty list and the existing accumulated value. The result is nothing
-- but the accumulated value with no further list element to apply the function
-- The recursive call takes the incremental accumulated value calculated so far from
-- the left to right of the list as new b value and the remaining tail of the list
myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl _ b [] = b
myFoldl f b (a:as) = myFoldl f (f b a) as

testMyFoldl :: Double
testMyFoldl = myFoldl (/) 24 [2,2,3]

-- 3. myFoldr :: (a -> b -> b) -> b -> [a] -> b
-- The edge condition of recursion occurs when any function is applied
-- on a empty list and the existing accumulated value. The result is nothing
-- but the accumulated value with no further list element to apply the function
-- The recursive call applies function f on the first element a and the 
-- recursively computed accumuated value from the remaining tail of the list
myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr _ b [] = b
myFoldr f b (a:as) = f a (myFoldr f b as)

testMyFoldr :: Double
testMyFoldr = myFoldr (/) 2 [12,8,4]


-- 4. myCycle :: [a] -> [a]
-- Recursive function to generate an infinite list by repeating all
-- elements in the input list in the same order.
myCycle :: [a] -> [a]
myCycle a = a ++ myCycle a

testMyCycle :: Bool
testMyCycle = take 10 (myCycle [5, 7]) == take 10 (cycle [5, 7])

-- 5. compose :: (b -> c) -> (a -> b) -> (a -> c)
compose :: (b -> c) -> (a -> b) -> (a -> c)
compose g f = (\x -> g(f(x)))

composeTest :: Num a => a -> a
composeTest = ((*2) `compose` (+1))

-- Returns a function that applies function f on each element of 
-- the input list and returns a list of tuples of the form (x, f(x))
functionPairs :: (t -> t) -> [t] -> [(t, t)]
functionPairs f = (\x -> map (\a -> (a, f(a))) x)

f1 x = x^2
sqrPairs = functionPairs f1
testFunctionPairs = sqrPairs [1..10]


-- 6. while :: state -> (state -> Bool) -> (state -> state) -> state
-- While function that behaves like a typical while loop till 
-- the given condition holds
-- As long as the current test (shouldContinue state) is true
-- the function recursively calls itself with the new state
-- obtained after applying the bodyFn
while :: state -> (state -> Bool) -> (state -> state) -> state
while state shouldContinue bodyFn
    | shouldContinue state = while (bodyFn state) shouldContinue bodyFn
    | otherwise = state

-- Test code to test the while function.
-- The state is the tuple (current index^2, [list of squares so far])
-- Initial state is (1, [])
-- The shouldContinue here is the lambda with test "index <= n"
-- The bodyFn generates the next state/tuple after appending
-- current index^2 to the list and also incrementing the index 
nSquares:: Int -> [Int]
nSquares n =
    reverse . snd $ -- Get the second element of the final state and reverse it.    
    while (1, [])
    (\(index, _) -> index <= n) -- n is the argument.
    (\(index, list) -> (index + 1, index^2 : list))

testWhile :: [Int]
testWhile = nSquares 15

-- Computes first n Fibonacci number recursively. 
-- nFibs 0 and nFibs 1 are recursion edge cases.
-- Start state is first 2 numbers of Fibonacci series and the result list
-- While loop test condtion is 'length(l) < n'
-- The next state is the new first, second numbers where first = second
-- and second = first + second and new list = list with (first+seconds)
-- appended to its end.
nFibs :: Int -> [Int]
nFibs 0 = []
nFibs 1 = [0]
nFibs n =
    (\(_,_,x) -> x) $
    while (0, 1, [0, 1])
    (\(f, s, l) -> length(l) < n)
    (\(f, s, l) -> (s, f+s, l++[f+s]))

testNFibs :: [Int]
testNFibs = nFibs 10

-- nPrimes computes first n prime number (not prime numbers upto n)
-- The function executes a while loop till the number of primes found 
-- so far is the less than the total number of primes to be generated.
-- The start state of the while loop is the prime number 3 and the list 
-- of primes found so far.
-- The function calls getNextPrimeBySieve to get the next prime number 
-- after n and generates the next state as (nextPrime, new list of primes)
nPrimes :: Int -> [Int]
nPrimes 0 = []
nPrimes 1 = [2]
nPrimes n = 
    snd $
    while (3, [2])  
    (\(i, p) -> length(p) < n)
    (\(i, p) -> (getNextPrimeBySieve (i+1) p, p ++ [i]))

-- Finds the next prime number starting from n by applying
-- Sieve of Eratosthenes algorithm
-- https://en.wikipedia.org/wiki/Sieve_of_Eratosthenes
getNextPrimeBySieve :: Int -> [Int] -> Int
getNextPrimeBySieve n p = head [x | x <- [n..], 
    x `notElem` concat [[y*y, y*y+y.. x] | y <- takeWhile (<=round(sqrt(fromIntegral x))) p]]     
--getNextPrimeBySieve n p = head [x | x <- [n..], 
--    0 `notElem` map (\y -> (x-y^2) `mod` y) (takeWhile (<=round(sqrt(fromIntegral x))) p)]     
----------------------------------------------------------------------------------
-- nPrimes n = 
--     snd $
--     while (3, [2])  
--     (\(i, p) -> length(p) < n)
--     (\(i, p) -> (i + 1, p ++ 
--     [i | 0 `notElem` [i `mod` x | x <- takeWhile (<=round(sqrt(fromIntegral i))) p]]))
----------------------------------------------------------------------------------
-- nPrimes n = 
--     snd $
--     while (3, [2])
--     (\(i, p) -> length(p) < n)
--     (\(i, p) -> (i + 1, p ++ [i | i `notElem` concat [[x * x, x * x + x .. i] | 
--     x <- takeWhile (<=round(sqrt(fromIntegral i))) p]]))
----------------------------------------------------------------------------------
-- nPrimes 0 = []
-- nPrimes 1 = [2]
-- nPrimes n = 
--     snd $
--     while (3, [2])
--     (\(i, p) -> length(p) < n)
--     (\(i, p) -> (i + 1, p ++ [i | i `notElem` concat [[x * x, x * x + x .. i] | 
--     x <- p]]))

doubleLoop :: Int -> Int -> [(Int, Int)]
doubleLoop outer inner = [(oIndex, iIndex) | oIndex <- [1..outer], iIndex <- [1..inner]]

-- double loop implementation using the while function
-- Outer while incrementally goes over from counter 1..outer
-- The body of the while calls the innerWhile, that for each
-- outer counter value, generates a list from 1..inner
-- Each value generated in inner while is combined with outer counter
-- to form the resulting tuple 
doubleWhile :: Int -> Int -> [(Int, Int)]
doubleWhile outer inner =
    tail $
    while [(0,0)]
    (\a -> fst (last a) < outer)
    (\a -> a ++ [(fst (last a) + 1, j) | j <- innerWhile inner])
    where
        innerWhile inner' = while [1]
            (\a -> last a < inner')
            (\a -> a ++ [last a + 1])


doubleWhileTest :: Bool
doubleWhileTest = doubleWhile 4 6 == doubleLoop 4 6            
