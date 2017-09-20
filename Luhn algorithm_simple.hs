--Returns lastDigit of number passed in argument.
lastDigit :: Integer -> Integer
lastDigit 0 = 0
lastDigit n = n `mod` 10

--Remove the last digit of the number passed in arugument.
dropLastDigit :: Integer -> Integer
dropLastDigit 0 = 0
dropLastDigit n = (n - (lastDigit n)) `div` 10

--Return a list of numbers which has digits in reverse order of number passed in the arguemnt.
toRevDigits :: Integer -> [Integer]
toRevDigits 0 = []
toRevDigits n = lastDigit n:(toRevDigits (dropLastDigit n))

--Returns a list by multiplying every other numbers of the list passed in argument.
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther [x] = [x]
--doubleEveryOther (x:y:[]) = [x,y*2]
doubleEveryOther (x:y:xs) = x:y*2:doubleEveryOther xs

--Returns an Integer which is sum of every member of the list passed in argument.
sumDigits :: [Integer] -> Integer
sumDigits [] = 0 
sumDigits (x:xs)
          | ((length (show x)) == 1) = x + sumDigits xs
          | otherwise = sumBigNumber x + sumDigits xs

--Returns sum of digits of number with more than one digit.
sumBigNumber :: Integer -> Integer
sumBigNumber x = lastDigit x + (sumDigits [dropLastDigit x])

--Returns True if Credit card number is valid based on Luhn's algorithm.
checkCC :: Integer -> Bool
checkCC x = sumDigits (doubleEveryOther (toRevDigits x)) `mod` 10 == 0 