--Returns the list with digits of number passed in the argument.
toDigits :: Int -> [Int]
toDigits n = map (\c -> read [c]) (show n)

--Returns a list by doubling the value of every other number in the list passed in argument.
doubleEveryOther :: [Int] -> [Int]
doubleEveryOther ds = zipWith (*) (reverse ds) (cycle [1,2])

--Returns sum of digits of list.
--If list contains two digits numbers, it first splits number into single digits, concat them and apply sum.
sumDigits :: [Int] -> Int
sumDigits = sum . concat . map toDigits

--Return checksum value of credit card number.
checkSum :: Int -> Int
checkSum = sumDigits . doubleEveryOther . toDigits

--Returns True if Credit card number statify luhn algorithm condition.
isValid :: Int -> Bool
isValid n = checkSum n `mod` 10 == 0

--Returns True if credit card number is valid.
testCC :: [Bool]
testCC = map isValid [79927398713, 79927398714] -- => [True, False]