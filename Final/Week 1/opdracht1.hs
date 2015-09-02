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
doubleEveryOther (x:xs) = 2 * x : doubleEveryOther xs 

evenIndex :: [Integer] -> [Integer]
evenIndex (x:xs) = if even then x : evenIndex xs else evenIndex xs 
