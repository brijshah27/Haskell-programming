myMap1 :: (a -> b) -> [a] -> [b] -> [b]
myMap1 _ [] acc= reverse acc
myMap1 f (x:xs) acc=  myMap1 f xs (f x:acc) 
--Possible question: What would happen if we reversed the order of the two myMap1 clauses?:l 
--Possible question: Is myMap1 tail recursive? Why or why not?

myMap2 :: (a -> b) -> [a] -> [b]
myMap2 f xs= reverse $ foldl (\accum x -> (f x : accum)) [] xs
--Possible question: Show how this works by stepping through the execution.

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith _ _ [] = []
myZipWith _ [] _ = []
myZipWith f (x:xs) (y:ys) = f x y : myZipWith f xs ys
--Possible question: If you use recursion, does the order of the clauses matter? Why or why not?

myFoldl :: (b -> a -> b) -> b -> [a] -> b 
myFoldl _ b [] = b
myFoldl f b (a:as) = myFoldl f (f b a) as
--Possible question: This is tail recursive. Why or why not? 
--Possible question: What is the relationship between the value produced by the base case and the initial function call? 
--That is, assume you make a call like this:
-- > myFoldl fn accInit list
--and assume that when the base case is reached it returns value
--What is the relationship (if any) between value and myFoldl fn accInit list?

myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr _ b [] = b
myFoldr f b (a:as) = f a (myFoldr f b as)
--Possible question: This is tail recursive. Why or why not? 
--Possible question: What is the relationship between the value produced by the base case and the initial function call?

myCycle :: [a] -> [a]
myCycle a = a ++ myCycle a
--Possible question: Why doesn’t this lead to an infinite loop?
--Possible question: What happens with the following? Explain why.
cyc12 = myCycle [1,2]
-- > take 5 cyc12
--Possible question: Walk through the step-by-step execution of 
-- > take 5 (myCycle [1,2])

compose :: (b -> c) -> (a -> b) -> (a -> c)
compose g f = (\x -> g(f(x)))
--Possible question: What is the type of h?

functionPairs :: (a -> b) -> [a] -> [(a, b)]
--functionPairs f = (\x -> map (\a -> (a, f(a))) x)
functionPairs f xs = map (\x -> (x, f(x))) xs
functionPairs f xs = zip xs (map f xs)

f1 x = x^2
sqrPairs = functionPairs f1
testFunctionPairs = sqrPairs [1..10]
--Possible question: Is your while function tail recursive. Why or why not?
--Possible question: Explain how nSquares works. What is the state? What do its components represent? (The answers are above.)

while :: state -> (state -> Bool) -> (state -> state) -> state
while state shouldContinue bodyFn
    | shouldContinue state = while (bodyFn state) shouldContinue bodyFn
    | otherwise = state

nSquares:: Int -> [Int]
nSquares n =
    reverse . snd $ -- Get the second element of the final state and reverse it.    
    while (1, [])
    (\(index, _) -> index <= n) -- n is the argument.
    (\(index, list) -> (index + 1, index^2 : list))

testWhile :: [Int]
testWhile = nSquares 15



