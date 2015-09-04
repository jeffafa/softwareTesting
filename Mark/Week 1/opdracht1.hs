-- Exercise 1 --
lastDigit :: Integer -> Integer
lastDigit n = head (reverse (digits n))  

dropLastDigit :: Integer -> Integer 
dropLastDigit n = fromDigits (reverse (tail (reverse (digits n)))) 

digits :: Integer -> [Integer]
digits = map (read . return) . show

fromDigits = foldl addDigit 0
   where addDigit num d = 10*num + d
		 
-- Exercise 1 tests ---
testLastDigit :: (Integer, Integer) -> Bool
testLastDigit (n, d) = lastDigit n == d

testDropLastDigit :: (Integer, Integer) -> Bool
testDropLastDigit (n, d) = dropLastDigit n == d
			 
-- Exercise 2 --
toRevDigits :: Integer -> [Integer] 
toRevDigits n = if n <= 0 then [] else reverse (digits n)

-- Exercise 2 tests-
testToRevDigits :: (Integer, [Integer]) -> Bool
testToRevDigits (n,d) = toRevDigits n == d

-- Exercise 3 -- 
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = zipWith (*) (cycle [1,2])

--Exercise 3 Tests---
testDoubleEveryOther :: ([Integer], [Integer]) -> Bool
testDoubleEveryOther (n,d) = doubleEveryOther n == d

--Exercise 4--
sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) = sum (digits x) + sumDigits xs  

--Exercise 4 Tests--- 
testSumDigits:: ([Integer], Integer) -> Bool
testSumDigits (n,d) = (sumDigits n) == d

--Exercise 5--
luhn :: Integer -> Bool
luhn x = (sumDigits (doubleEveryOther (toRevDigits x))) `mod` 10 == 0

--Exercise 5 Tests--
testLuhn:: (Integer, Bool) -> Bool
testLuhn (n,d) = (luhn n) == d

